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
import forms.previousRegistrations.PreviousOssNumberFormProvider
import models.domain.PreviousSchemeNumbers
import models.previousRegistrations.{NonCompliantDetails, PreviousSchemeHintText, SchemeDetailsWithOptionalVatNumber}
import models.requests.DataRequest
import models.{Country, CountryWithValidationDetails, Index, PreviousScheme, WithName}
import pages.Waypoints
import pages.previousRegistrations.{PreviousOssNumberPage, PreviousSchemePage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import queries.previousRegistrations.AllPreviousSchemesForCountryWithOptionalVatNumberQuery
import services.core.CoreRegistrationValidationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.AmendWaypoints.AmendWaypointsOps
import utils.FutureSyntax.*
import views.html.previousRegistrations.PreviousOssNumberView

import java.time.Clock
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PreviousOssNumberController @Inject()(
                                             override val messagesApi: MessagesApi,
                                             cc: AuthenticatedControllerComponents,
                                             formProvider: PreviousOssNumberFormProvider,
                                             view: PreviousOssNumberView,
                                             coreRegistrationValidationService: CoreRegistrationValidationService,
                                             clock: Clock
                                           )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport with GetCountry with PreviousNonComplianceAnswers {

  protected val controllerComponents: MessagesControllerComponents = cc

  def onPageLoad(waypoints: Waypoints, countryIndex: Index, schemeIndex: Index): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>
      getPreviousCountry(waypoints, countryIndex) {
        country =>
          val maybeCurrentAnswer = request.userAnswers.get(PreviousOssNumberPage(countryIndex, schemeIndex))

          val (isEditingAndAnotherOssScheme, form) = request.userAnswers.get(AllPreviousSchemesForCountryWithOptionalVatNumberQuery(countryIndex)) match {
            case Some(previousSchemeDetails) =>
              getFormAndIfEditingExistingWithSecondaryScheme(countryIndex, schemeIndex, request, country, maybeCurrentAnswer, previousSchemeDetails)
            case None =>
              (false, formProvider(country, Seq.empty))
          }

          val previousSchemeHintText = determinePreviousSchemeHintText(countryIndex, maybeCurrentAnswer.isDefined && !isEditingAndAnotherOssScheme)

          val preparedForm = maybeCurrentAnswer match
            case None => form
            case Some(value) => form.fill(value.previousSchemeNumber)

          CountryWithValidationDetails.euCountriesWithVRNValidationRules.find(_.country.code == country.code) match
            case Some(countryWithValidationDetails) =>
              Ok(view(preparedForm, waypoints, countryIndex, schemeIndex, countryWithValidationDetails, previousSchemeHintText)).toFuture
            case _ =>
              throw new RuntimeException(s"Cannot find country code ${country.code} in euCountriesWithVRNValidationRules")
      }
  }

  private def getFormAndIfEditingExistingWithSecondaryScheme(
                                                              countryIndex: Index,
                                                              schemeIndex: Index,
                                                              request: DataRequest[AnyContent],
                                                              country: Country,
                                                              maybeCurrentAnswer: Option[PreviousSchemeNumbers],
                                                              previousSchemeDetails: List[SchemeDetailsWithOptionalVatNumber]
                                                            ): (Boolean, Form[String]) = {

    val previousSchemes = previousSchemeDetails.flatMap(_.previousScheme)
    val filteredSchemes = request.userAnswers.get(PreviousSchemePage(countryIndex, schemeIndex)) match {
      case Some(currentSchemeType) => previousSchemes.filterNot(_ == currentSchemeType)
      case None => previousSchemes
    }

    val isEditing = maybeCurrentAnswer.isDefined
    val schemeCountOfSameType = previousSchemes.count {
      case scheme if maybeCurrentAnswer.isDefined =>
        request.userAnswers.get(PreviousSchemePage(countryIndex, schemeIndex)).contains(scheme)
      case _ => false
    }
    val isEditingAndAnotherOssScheme = isEditing && schemeCountOfSameType > 1

    (isEditingAndAnotherOssScheme, formProvider(country, filteredSchemes))
  }

  private def determinePreviousSchemeHintText(
                                               countryIndex: Index,
                                               hasCurrentAnswer: Boolean
                                             )(implicit request: DataRequest[AnyContent]): PreviousSchemeHintText = {
    if (hasCurrentAnswer) {
      PreviousSchemeHintText.Both
    } else {
      request.userAnswers.get(AllPreviousSchemesForCountryWithOptionalVatNumberQuery(countryIndex)) match {
        case Some(listSchemeDetails) =>
          val previousSchemes = listSchemeDetails.flatMap(_.previousScheme)
          if (previousSchemes.contains(PreviousScheme.OSSU)) {
            PreviousSchemeHintText.OssNonUnion
          } else if (previousSchemes.contains(PreviousScheme.OSSNU)) {
            PreviousSchemeHintText.OssUnion
          } else {
            PreviousSchemeHintText.Both
          }
        case _ => PreviousSchemeHintText.Both
      }
    }
  }

  def onSubmit(waypoints: Waypoints, countryIndex: Index, schemeIndex: Index): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>
      getPreviousCountry(waypoints, countryIndex) {
        country =>

          val maybeCurrentAnswer = request.userAnswers.get(PreviousOssNumberPage(countryIndex, schemeIndex))

          val (isEditingAndAnotherOssScheme, form) = request.userAnswers.get(AllPreviousSchemesForCountryWithOptionalVatNumberQuery(countryIndex)) match
            case Some(previousSchemeDetails) =>
              getFormAndIfEditingExistingWithSecondaryScheme(countryIndex, schemeIndex, request, country, maybeCurrentAnswer, previousSchemeDetails)
            case None =>
              (false, formProvider(country, Seq.empty))

          val previousSchemeHintText = determinePreviousSchemeHintText(countryIndex, maybeCurrentAnswer.isDefined && !isEditingAndAnotherOssScheme)

          form.bindFromRequest().fold(
            formWithErrors =>
              CountryWithValidationDetails.euCountriesWithVRNValidationRules.filter(_.country.code == country.code).head match {
                case countryWithValidationDetails =>
                  Future.successful(BadRequest(view(
                    formWithErrors, waypoints, countryIndex, schemeIndex, countryWithValidationDetails, previousSchemeHintText)))
              },

            value => {
              val previousScheme = if (value.startsWith("EU")) {
                PreviousScheme.OSSNU
              } else {
                PreviousScheme.OSSU
              }
              searchSchemeThenSaveAndRedirect(waypoints, countryIndex, schemeIndex, country, value, previousScheme)
            }
          )
      }
  }

  private def searchSchemeThenSaveAndRedirect(
                                               waypoints: Waypoints,
                                               countryIndex: Index,
                                               schemeIndex: Index,
                                               country: Country,
                                               value: String,
                                               previousScheme: WithName with PreviousScheme
                                             )(implicit request: DataRequest[AnyContent]): Future[Result] = {

    coreRegistrationValidationService.searchScheme(
      searchNumber = value,
      previousScheme = previousScheme,
      intermediaryNumber = None,
      countryCode = country.code
    ).flatMap {

      case Some(activeMatch) if activeMatch.isQuarantinedTrader(clock) =>
        Redirect(
          controllers.routes.OtherCountryExcludedAndQuarantinedController.onPageLoad(
            activeMatch.memberState,
            activeMatch.getEffectiveDate)
        ).toFuture

      case Some(activeMatch) =>
        saveAndRedirect(
          countryIndex,
          schemeIndex,
          value,
          previousScheme,
          waypoints,
          Some(NonCompliantDetails(nonCompliantReturns = activeMatch.nonCompliantReturns, nonCompliantPayments = activeMatch.nonCompliantPayments))
        )
      case _ =>
        saveAndRedirect(countryIndex, schemeIndex, value, previousScheme, waypoints, None)
    }

  }

  private def saveAndRedirect(
                               countryIndex: Index,
                               schemeIndex: Index,
                               registrationNumber: String,
                               previousScheme: PreviousScheme,
                               waypoints: Waypoints,
                               maybeNonCompliantDetails: Option[NonCompliantDetails]
                             )(implicit request: DataRequest[AnyContent]): Future[Result] = {
    for {
      updatedAnswers <- Future.fromTry(request.userAnswers.set(
        PreviousOssNumberPage(countryIndex, schemeIndex),
        PreviousSchemeNumbers(registrationNumber),
      ))
      updatedAnswersWithScheme <- Future.fromTry(updatedAnswers.set(
        PreviousSchemePage(countryIndex, schemeIndex),
        previousScheme
      ))
      updatedAnswersWithNonCompliantDetails <- setNonCompliantDetailsAnswers(
        countryIndex,
        schemeIndex,
        maybeNonCompliantDetails,
        updatedAnswersWithScheme
      )
      _ <- cc.sessionRepository.set(updatedAnswersWithNonCompliantDetails)
    } yield Redirect(PreviousOssNumberPage(countryIndex, schemeIndex).navigate(waypoints, request.userAnswers, updatedAnswersWithNonCompliantDetails).route)
  }

}
