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

import controllers.GetCountry
import controllers.actions.*
import forms.previousRegistrations.PreviousSchemeTypeFormProvider
import models.Index
import pages.Waypoints
import pages.previousRegistrations.PreviousSchemeTypePage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.previousRegistrations.AllPreviousSchemesForCountryWithOptionalVatNumberQuery
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.AmendWaypoints.AmendWaypointsOps
import utils.FutureSyntax.*
import views.html.previousRegistrations.PreviousSchemeView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PreviousSchemeController @Inject()(
                                          override val messagesApi: MessagesApi,
                                          cc: AuthenticatedControllerComponents,
                                          formProvider: PreviousSchemeTypeFormProvider,
                                          view: PreviousSchemeView
                                        )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport with GetCountry {

  protected val controllerComponents: MessagesControllerComponents = cc

  def onPageLoad(waypoints: Waypoints, countryIndex: Index, schemeIndex: Index): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>
      getPreviousCountry(waypoints, countryIndex) {
        country =>

          val form = request.userAnswers.get(AllPreviousSchemesForCountryWithOptionalVatNumberQuery(countryIndex)) match {
            case Some(previousSchemesDetails) =>

              val previousSchemes = previousSchemesDetails.flatMap(_.previousScheme)
              formProvider(country.name, previousSchemes, schemeIndex)

            case None =>
              formProvider(country.name, Seq.empty, schemeIndex)
          }

          val preparedForm = request.userAnswers.get(PreviousSchemeTypePage(countryIndex, schemeIndex)) match {
            case None => form
            case Some(value) => form.fill(value)
          }

          Ok(view(preparedForm, waypoints, countryIndex, schemeIndex)).toFuture
      }
  }

  def onSubmit(waypoints: Waypoints, countryIndex: Index, schemeIndex: Index): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>
      getPreviousCountry(waypoints, countryIndex) {
        country =>

          val form = request.userAnswers.get(AllPreviousSchemesForCountryWithOptionalVatNumberQuery(countryIndex)) match {
            case Some(previousSchemesDetails) =>

              val previousSchemes = previousSchemesDetails.flatMap(_.previousScheme)
              formProvider(country.name, previousSchemes, schemeIndex)

            case None =>
              formProvider(country.name, Seq.empty, schemeIndex)
          }

          form.bindFromRequest().fold(
            formWithErrors =>
              BadRequest(view(formWithErrors, waypoints, countryIndex, schemeIndex)).toFuture,

            value =>
              for {
                updatedAnswers <- Future.fromTry(request.userAnswers.set(PreviousSchemeTypePage(countryIndex, schemeIndex), value))
                _ <- cc.sessionRepository.set(updatedAnswers)
              } yield Redirect(PreviousSchemeTypePage(countryIndex, schemeIndex).navigate(waypoints, request.userAnswers, updatedAnswers).route)
          )
      }
  }
}
