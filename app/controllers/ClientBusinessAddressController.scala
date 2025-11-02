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
import forms.ClientBusinessAddressFormProvider
import models.InternationalAddress
import pages.{BusinessBasedInUKPage, ClientBusinessAddressPage, Waypoints}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.FutureSyntax.FutureOps
import views.html.ClientBusinessAddressView
import utils.AmendWaypoints.AmendWaypointsOps

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ClientBusinessAddressController @Inject()(
                                        override val messagesApi: MessagesApi,
                                        cc: AuthenticatedControllerComponents,
                                        formProvider: ClientBusinessAddressFormProvider,
                                        view: ClientBusinessAddressView
                                    )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport with GetCountry {

  protected val controllerComponents: MessagesControllerComponents = cc

  def onPageLoad(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetData(waypoints.inAmend).async {
    implicit request =>

      val isUKBased = request.userAnswers.get(BusinessBasedInUKPage).getOrElse(false)

      if (isUKBased) {

        val form = formProvider(None)

        val preparedForm = request.userAnswers.get(ClientBusinessAddressPage) match {
          case None => form
          case Some(value) => form.fill(value)
        }

        Ok(view(preparedForm, waypoints, None)).toFuture
        
      } else {
        getCountry(waypoints) { country =>

          val form: Form[InternationalAddress] = formProvider(Some(country))

          val preparedForm = request.userAnswers.get(ClientBusinessAddressPage) match {
            case None => form
            case Some(value) => form.fill(value)
          }

          Ok(view(preparedForm, waypoints, Some(country))).toFuture
        }
      }
      
  }

  def onSubmit(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetData(waypoints.inAmend).async {
    implicit request =>

      val isUKBased = request.userAnswers.get(BusinessBasedInUKPage).getOrElse(false)

      if (isUKBased) {
        val form = formProvider(None)

        form.bindFromRequest().fold(
          formWithErrors =>
            BadRequest(view(formWithErrors, waypoints, None)).toFuture,

          value =>
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(ClientBusinessAddressPage, value))
              _ <- cc.sessionRepository.set(updatedAnswers)
            } yield Redirect(ClientBusinessAddressPage.navigate(waypoints, request.userAnswers, updatedAnswers).route)
        )
        
      } else {

        getCountry(waypoints) { country =>

          val form: Form[InternationalAddress] = formProvider(Some(country))

          form.bindFromRequest().fold(
            formWithErrors =>
              BadRequest(view(formWithErrors, waypoints, Some(country))).toFuture,

            value =>
              for {
                updatedAnswers <- Future.fromTry(request.userAnswers.set(ClientBusinessAddressPage, value))
                _ <- cc.sessionRepository.set(updatedAnswers)
              } yield Redirect(ClientBusinessAddressPage.navigate(waypoints, request.userAnswers, updatedAnswers).route)
          )
        }
      }
  }
}
