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
import forms.previousRegistrations.PreviousIossNumberFormProvider
import logging.Logging
import models.PreviousScheme.IOSSWOI
import models.domain.PreviousSchemeNumbers
import models.previousRegistrations.{IossRegistrationNumberValidation, NonCompliantDetails}
import models.requests.DataRequest
import models.{Country, Index, PreviousScheme}
import pages.Waypoints
import pages.previousRegistrations.{ClientHasIntermediaryPage, PreviousIossNumberPage, PreviousSchemePage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import services.core.CoreRegistrationValidationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.AmendWaypoints.AmendWaypointsOps
import utils.FutureSyntax.FutureOps
import views.html.previousRegistrations.PreviousIossNumberView

import java.time.Clock
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PreviousIossNumberController @Inject()(
                                              override val messagesApi: MessagesApi,
                                              cc: AuthenticatedControllerComponents,
                                              formProvider: PreviousIossNumberFormProvider,
                                              view: PreviousIossNumberView,
                                              coreRegistrationValidationService: CoreRegistrationValidationService,
                                              clock: Clock
                                            )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport with Logging with GetCountry with PreviousNonComplianceAnswers {

  protected val controllerComponents: MessagesControllerComponents = cc


  def onPageLoad(waypoints: Waypoints, countryIndex: Index, schemeIndex: Index): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>
      getPreviousCountry(waypoints, countryIndex) { country =>
        val form = formProvider(country)

        val preparedForm = request.userAnswers.get(PreviousIossNumberPage(countryIndex, schemeIndex)) match {
          case None => form
          case Some(value) => form.fill(value.previousSchemeNumber)
        }

        Ok(view(preparedForm, waypoints, countryIndex, schemeIndex, country, getIossHintText(country))).toFuture
      }
  }

  def onSubmit(waypoints: Waypoints, countryIndex: Index, schemeIndex: Index): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>
      getPreviousCountry(waypoints, countryIndex) { country =>

        val form = formProvider(country)
        form.bindFromRequest().fold(
          formWithErrors =>
            BadRequest(view(
              formWithErrors, waypoints, countryIndex, schemeIndex, country, getIossHintText(country)
            )).toFuture,

          value =>
            coreRegistrationValidationService.searchScheme(
              searchNumber = value,
              previousScheme = IOSSWOI,
              intermediaryNumber = None,
              countryCode = country.code
            ).flatMap {
              case Some(activeMatch) if activeMatch.isActiveTrader =>
                Redirect(controllers.routes.ClientAlreadyRegisteredController.onPageLoad()).toFuture

              case Some(activeMatch) if activeMatch.isQuarantinedTrader(clock) =>
                Redirect(
                  controllers.routes.OtherCountryExcludedAndQuarantinedController.onPageLoad(
                    activeMatch.memberState,
                    activeMatch.getEffectiveDate)
                ).toFuture

              case Some(activeMatch) =>
                submitAnswersAndRedirect(
                  waypoints,
                  countryIndex,
                  schemeIndex,
                  value,
                  Some(NonCompliantDetails(nonCompliantReturns = activeMatch.nonCompliantReturns, nonCompliantPayments = activeMatch.nonCompliantPayments))
                )
              case _ =>
                submitAnswersAndRedirect(waypoints, countryIndex, schemeIndex, value, None)
            }
        )
      }
  }

  private def submitAnswersAndRedirect(
                                        waypoints: Waypoints,
                                        countryIndex: Index,
                                        schemeIndex: Index,
                                        value: String,
                                        maybeNonCompliantDetails: Option[NonCompliantDetails]
                                      )(implicit request: DataRequest[_]) = {
    for {
      updatedAnswers <- Future.fromTry(request.userAnswers.set(PreviousIossNumberPage(countryIndex, schemeIndex), PreviousSchemeNumbers(value)))
      updatedAnswersWithPreviousScheme <- Future.fromTry(updatedAnswers.set(
        PreviousSchemePage(countryIndex, schemeIndex), determinePreviousScheme(countryIndex, schemeIndex)
      ))
      updatedAnswersWithNonCompliantDetails <- setNonCompliantDetailsAnswers(
        countryIndex,
        schemeIndex,
        maybeNonCompliantDetails,
        updatedAnswersWithPreviousScheme
      )
      _ <- cc.sessionRepository.set(updatedAnswersWithNonCompliantDetails)
    } yield Redirect(PreviousIossNumberPage(countryIndex, schemeIndex).navigate(waypoints, request.userAnswers, updatedAnswersWithNonCompliantDetails).route)
  }

  private def getIossHintText(country: Country): String = {
    IossRegistrationNumberValidation.euCountriesWithIOSSValidationRules.filter(_.country == country).head match {
      case countryWithIossValidation => countryWithIossValidation.messageInput
    }
  }

  private def determinePreviousScheme(
                                       countryIndex: Index,
                                       schemeIndex: Index
                                     )(implicit request: DataRequest[_]): PreviousScheme = {
    request.userAnswers.get(ClientHasIntermediaryPage(countryIndex, schemeIndex)) match {
      case Some(true) => PreviousScheme.IOSSWI
      case _ => PreviousScheme.IOSSWOI
    }
  }
}

