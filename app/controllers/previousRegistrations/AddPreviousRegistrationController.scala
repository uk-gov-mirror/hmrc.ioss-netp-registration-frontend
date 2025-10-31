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
import forms.previousRegistrations.AddPreviousRegistrationFormProvider
import logging.Logging
import models.Country
import models.domain.PreviousRegistration
import models.etmp.EtmpPreviousEuRegistrationDetails
import models.previousRegistrations.PreviousRegistrationDetailsWithOptionalVatNumber
import models.requests.DataRequest
import pages.previousRegistrations.AddPreviousRegistrationPage
import pages.{JourneyRecoveryPage, Waypoints}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import queries.previousRegistrations.DeriveNumberOfPreviousRegistrations
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.AmendWaypoints.AmendWaypointsOps
import utils.CompletionChecks
import utils.FutureSyntax.*
import utils.ItemsHelper.getDerivedItems
import utils.PreviousRegistrationsCompletionChecks.{getAllIncompleteRegistrationDetails, incompletePreviousRegistrationRedirect}
import viewmodels.previousRegistrations.PreviousRegistrationSummary
import views.html.previousRegistrations.AddPreviousRegistrationView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AddPreviousRegistrationController @Inject()(
                                                   override val messagesApi: MessagesApi,
                                                   cc: AuthenticatedControllerComponents,
                                                   formProvider: AddPreviousRegistrationFormProvider,
                                                   view: AddPreviousRegistrationView
                                                 )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport with Logging with CompletionChecks {

  protected val controllerComponents: MessagesControllerComponents = cc

  val form: Form[Boolean] = formProvider()

  def onPageLoad(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>

      getDerivedItems(waypoints, DeriveNumberOfPreviousRegistrations) { number =>

        val previousExistingRegistrations: Seq[PreviousRegistration] = getAllExistingPreviousRegistrationInAmend(waypoints)

        val canAddCountries = number < Country.euCountries.size

        val previousRegistrations = PreviousRegistrationSummary.row(
          answers = request.userAnswers,
          existingPreviousRegistrations = previousExistingRegistrations,
          waypoints = waypoints,
          sourcePage = AddPreviousRegistrationPage()
        )

        val failureCall: Seq[PreviousRegistrationDetailsWithOptionalVatNumber] => Future[Result] =
          (incomplete: Seq[PreviousRegistrationDetailsWithOptionalVatNumber]) => {
            Ok(view(form, waypoints, previousRegistrations, canAddCountries, incomplete)).toFuture
          }

        withCompleteDataAsync[PreviousRegistrationDetailsWithOptionalVatNumber](
          data = () => getAllIncompleteRegistrationDetails(), onFailure = failureCall) {
          Ok(view(form, waypoints, previousRegistrations, canAddCountries)).toFuture
        }
      }

  }

  def onSubmit(waypoints: Waypoints, incompletePromptShown: Boolean): Action[AnyContent] = cc.identifyAndGetData(inAmend = waypoints.inAmend).async {
    implicit request =>
      withCompleteDataAsync[PreviousRegistrationDetailsWithOptionalVatNumber](
        data = getAllIncompleteRegistrationDetails _,
        onFailure = (_: Seq[PreviousRegistrationDetailsWithOptionalVatNumber]) => {
          if (incompletePromptShown) {
            incompletePreviousRegistrationRedirect(waypoints).map(
              redirectIncompletePage => redirectIncompletePage.toFuture
            ).getOrElse(Redirect(JourneyRecoveryPage.route(waypoints).url).toFuture)
          } else {
            Redirect(routes.AddPreviousRegistrationController.onPageLoad(waypoints)).toFuture
          }
        }) {
        getDerivedItems(waypoints, DeriveNumberOfPreviousRegistrations) { number =>

          val canAddCountries = number < Country.euCountries.size

          val previousRegistrations = PreviousRegistrationSummary.row(
            answers = request.userAnswers,
            existingPreviousRegistrations = Seq.empty,
            waypoints = waypoints,
            sourcePage = AddPreviousRegistrationPage()
          )

          form.bindFromRequest().fold(
            formWithErrors =>
              BadRequest(view(formWithErrors, waypoints, previousRegistrations, canAddCountries)).toFuture,

            (addAnotherRegistration: Boolean) =>
              for {
                updatedAnswers <- Future.fromTry(request.userAnswers.set(AddPreviousRegistrationPage(), addAnotherRegistration))
                _ <- cc.sessionRepository.set(updatedAnswers)
              } yield Redirect(AddPreviousRegistrationPage().navigate(
                waypoints,
                request.userAnswers,
                updatedAnswers).url
              )
          )
        }
      }
  }

  private def getAllExistingPreviousRegistrationInAmend(waypoints: Waypoints)(implicit request: DataRequest[_]) = {
    if (waypoints.inAmend) {
      val allExistingPreviousRegistrations: Seq[EtmpPreviousEuRegistrationDetails] = request.registrationWrapper
        .map(_.etmpDisplayRegistration.schemeDetails.previousEURegistrationDetails).toSeq.flatten

      allExistingPreviousRegistrations.map { existingPreviousRegistration =>
        val country: Country = Country.fromCountryCodeUnsafe(existingPreviousRegistration.issuedBy)

        PreviousRegistration.fromEtmpPreviousEuRegistrationDetailsByCountry(country, allExistingPreviousRegistrations)
      }
    } else {
      Seq.empty
    }
  }
}
