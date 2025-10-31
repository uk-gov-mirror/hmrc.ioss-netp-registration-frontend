/*
 * Copyright 2025 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers

import controllers.actions.*
import forms.ClientsNinoNumberFormProvider
import logging.Logging
import models.core.Match
import pages.{ClientsNinoNumberPage, Waypoints}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import services.core.CoreRegistrationValidationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.FutureSyntax.FutureOps
import views.html.ClientsNinoNumberView

import java.time.Clock
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ClientsNinoNumberController @Inject()(
                                             override val messagesApi: MessagesApi,
                                             cc: AuthenticatedControllerComponents,
                                             formProvider: ClientsNinoNumberFormProvider,
                                             view: ClientsNinoNumberView,
                                             coreRegistrationValidationService: CoreRegistrationValidationService,
                                             clock: Clock
                                           )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport with Logging with GetCountry {

  val form: Form[String] = formProvider()
  protected val controllerComponents: MessagesControllerComponents = cc

  def onPageLoad(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetData() {
    implicit request =>

      val preparedForm = request.userAnswers.get(ClientsNinoNumberPage) match {
        case None => form
        case Some(value) => form.fill(value)
      }

      Ok(view(preparedForm, waypoints))
  }

  def onSubmit(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetData().async {
    implicit request =>

      form.bindFromRequest().fold(
        formWithErrors =>
          Future.successful(BadRequest(view(formWithErrors, waypoints))),

        value =>
          coreRegistrationValidationService.searchTraderId(value).flatMap {

            case Some(activeMatch) if activeMatch.isActiveTrader =>
              Redirect(controllers.routes.ClientAlreadyRegisteredController.onPageLoad()).toFuture

            case Some(activeMatch) if activeMatch.isQuarantinedTrader(clock) =>
              Redirect(
                controllers.routes.OtherCountryExcludedAndQuarantinedController.onPageLoad(
                  activeMatch.memberState,
                  activeMatch.getEffectiveDate)
              ).toFuture

            case _ =>
              for {
                updatedAnswers <- Future.fromTry(request.userAnswers.set(ClientsNinoNumberPage, value))
                _ <- cc.sessionRepository.set(updatedAnswers)
              } yield Redirect(ClientsNinoNumberPage.navigate(waypoints, request.userAnswers, updatedAnswers).route)
          }
      )
  }
}
