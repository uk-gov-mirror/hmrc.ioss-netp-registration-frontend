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

package controllers.vatEuDetails

import controllers.GetCountry
import controllers.actions.*
import forms.vatEuDetails.EuTaxReferenceFormProvider
import models.Index
import models.requests.DataRequest
import pages.Waypoints
import pages.vatEuDetails.EuTaxReferencePage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import services.core.CoreRegistrationValidationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.AmendWaypoints.AmendWaypointsOps
import utils.FutureSyntax.FutureOps
import views.html.vatEuDetails.EuTaxReferenceView

import java.time.Clock
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class EuTaxReferenceController @Inject()(
                                          override val messagesApi: MessagesApi,
                                          cc: AuthenticatedControllerComponents,
                                          formProvider: EuTaxReferenceFormProvider,
                                          view: EuTaxReferenceView,
                                          coreRegistrationValidationService: CoreRegistrationValidationService,
                                          clock: Clock
                                        )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport with GetCountry {

  protected val controllerComponents: MessagesControllerComponents = cc

  def onPageLoad(waypoints: Waypoints, countryIndex: Index): Action[AnyContent] = cc.identifyAndGetData().async {
    implicit request =>

      getCountryWithIndex(waypoints, countryIndex) { country =>

        val form = formProvider(country)

        val preparedForm = request.userAnswers.get(EuTaxReferencePage(countryIndex)) match {
          case None => form
          case Some(value) => form.fill(value)
        }

        Ok(view(preparedForm, waypoints, countryIndex, country)).toFuture
      }
  }

  def onSubmit(waypoints: Waypoints, countryIndex: Index): Action[AnyContent] = cc.identifyAndGetData().async {
    implicit request =>

      getCountryWithIndex(waypoints, countryIndex) { country =>

        val form = formProvider(country)

        form.bindFromRequest().fold(
          formWithErrors =>
            Future.successful(BadRequest(view(formWithErrors, waypoints, countryIndex, country))),

          value =>
            coreRegistrationValidationService.searchEuTaxId(value, country.code).flatMap {

              case _ if waypoints.inAmend =>
                updateAnswersAndRedirect(waypoints, countryIndex, request, value)

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
                  updatedAnswers <- Future.fromTry(request.userAnswers.set(EuTaxReferencePage(countryIndex), value))
                  _ <- cc.sessionRepository.set(updatedAnswers)
                } yield Redirect(EuTaxReferencePage(countryIndex).navigate(waypoints, request.userAnswers, updatedAnswers).route)
            }
        )
      }
  }

  private def updateAnswersAndRedirect(waypoints: Waypoints, countryIndex: Index, request: DataRequest[AnyContent], euTaxRef: String) = {
    for {
      updatedAnswers <- Future.fromTry(request.userAnswers.set(EuTaxReferencePage(countryIndex), euTaxRef))
      _ <- cc.sessionRepository.set(updatedAnswers)
    } yield Redirect(EuTaxReferencePage(countryIndex).navigate(waypoints, request.userAnswers, updatedAnswers).route)
  }
}
