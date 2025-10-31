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

package controllers.previousRegistrations

import controllers.actions.*
import forms.previousRegistrations.PreviouslyRegisteredFormProvider
import pages.Waypoints
import pages.previousRegistrations.PreviouslyRegisteredPage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.previousRegistrations.{AllPreviousRegistrationsRawQuery, DeriveNumberOfPreviousRegistrations}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.AmendWaypoints.AmendWaypointsOps
import utils.CheckExistingRegistrations
import utils.FutureSyntax.*
import views.html.previousRegistrations.PreviouslyRegisteredView
import utils.CheckExistingRegistrations.cleanup

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PreviouslyRegisteredController @Inject()(
                                                override val messagesApi: MessagesApi,
                                                cc: AuthenticatedControllerComponents,
                                                formProvider: PreviouslyRegisteredFormProvider,
                                                view: PreviouslyRegisteredView
                                              )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport {

  private val form = formProvider()

  protected val controllerComponents: MessagesControllerComponents = cc

  def onPageLoad(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend) {
    implicit request =>
      val userAnswers = request.userAnswers

      val preparedForm = userAnswers.get(PreviouslyRegisteredPage) match {
        case None => form
        case Some(value) => form.fill(value)
      }

      Ok(view(preparedForm, waypoints))
  }

  def onSubmit(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>

      form.bindFromRequest().fold(
        formWithErrors =>
          BadRequest(view(formWithErrors, waypoints)).toFuture,

        value =>
          for {
            updatedAnswers <- Future.fromTry(request.userAnswers.set(PreviouslyRegisteredPage, value))
            finalAnswers <- Future.fromTry(cleanup(updatedAnswers, DeriveNumberOfPreviousRegistrations, AllPreviousRegistrationsRawQuery))
            _ <- cc.sessionRepository.set(finalAnswers)
          } yield Redirect(PreviouslyRegisteredPage.navigate(waypoints, request.userAnswers, finalAnswers).route)
      )
  }
}
