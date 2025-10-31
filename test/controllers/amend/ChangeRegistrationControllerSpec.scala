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

package controllers.amend

import base.SpecBase
import config.Constants.maxSchemes
import models.domain.{PreviousRegistration, PreviousSchemeDetails, VatCustomerInfo}
import models.etmp.amend.AmendRegistrationResponse
import models.etmp.display.EtmpDisplayRegistration
import models.previousRegistrations.NonCompliantDetails
import models.{CheckMode, ClientBusinessName, DesAddress, TradingName, UserAnswers}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{times, verify, when}
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar
import pages.amend.ChangeRegistrationPage
import pages.previousRegistrations.PreviouslyRegisteredPage
import pages.tradingNames.HasTradingNamePage
import pages.vatEuDetails.HasFixedEstablishmentPage
import pages.{BusinessBasedInUKPage, BusinessContactDetailsPage, ClientBusinessNamePage, ClientCountryBasedPage, ClientHasUtrNumberPage, ClientHasVatNumberPage, ClientTaxReferencePage, ClientUtrNumberPage, ClientVatNumberPage, EmptyWaypoints, Waypoint, Waypoints}
import play.api.i18n.Messages
import play.api.inject
import play.api.inject.bind
import play.api.test.Helpers.*
import play.api.test.{FakeRequest, Helpers}
import queries.euDetails.AllEuDetailsQuery
import queries.previousRegistrations.AllPreviousRegistrationsQuery
import queries.tradingNames.AllTradingNamesQuery
import queries.{IossNumberQuery, OriginalRegistrationQuery}
import services.RegistrationService
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import utils.FutureSyntax.FutureOps
import viewmodels.WebsiteSummary
import viewmodels.checkAnswers.*
import viewmodels.checkAnswers.tradingNames.{HasTradingNameSummary, TradingNameSummary}
import viewmodels.checkAnswers.vatEuDetails.{EuDetailsSummary, HasFixedEstablishmentSummary}
import viewmodels.govuk.SummaryListFluency
import viewmodels.govuk.all.SummaryListViewModel
import viewmodels.previousRegistrations.{PreviousRegistrationSummary, PreviouslyRegisteredSummary}
import views.html.ChangeRegistrationView

import java.time.{Instant, LocalDate, LocalDateTime}

class ChangeRegistrationControllerSpec extends SpecBase with SummaryListFluency with MockitoSugar with BeforeAndAfterEach {

  private val amendYourAnswersPage = ChangeRegistrationPage
  private val waypoints: Waypoints = EmptyWaypoints.setNextWaypoint(Waypoint(amendYourAnswersPage, CheckMode, amendYourAnswersPage.urlFragment))
  private val companyName: String = "Company Name"

  override val vatCustomerInfo: VatCustomerInfo = {
    VatCustomerInfo(
      registrationDate = LocalDate.now(),
      desAddress = DesAddress(
        line1 = "1818 East Tusculum Street",
        line2 = Some("Phil Tow"),
        line3 = None, line4 = None, line5 = None,
        postCode = Some("BT4 2XW"),
        countryCode = "EL"),
      organisationName = Some("Company name"),
      individualName = None,
      singleMarketIndicator = true,
      deregistrationDecisionDate = None
    )
  }

  val existingPreviousRegistrations: Seq[PreviousRegistration] = Gen.listOfN(2, arbitraryPreviousRegistration.arbitrary).sample.value

  val previousRegistrationDetails: PreviousRegistration = {
    PreviousRegistration(
      previousEuCountry = arbitraryCountry.arbitrary.sample.value,
      previousSchemesDetails = Gen.listOfN(
        maxSchemes, PreviousSchemeDetails(
          previousScheme = arbitraryPreviousScheme.arbitrary.sample.value,
          previousSchemeNumbers = arbitraryPreviousIossSchemeDetails.arbitrary.sample.value,
          nonCompliantDetails = Gen.option(NonCompliantDetails(
            Gen.option(Gen.choose(0, 2).sample.value).sample.value,
            Gen.option(Gen.choose(0, 2).sample.value).sample.value)
          ).sample.value
        )
      ).sample.value
    )
  }

  private val basicUserAnswersWithVatInfo: UserAnswers =
    UserAnswers(id = "12345-credId", vatInfo = Some(vatCustomerInfo), lastUpdated = Instant.now())
      .set(IossNumberQuery, iossNumber).success.value

  private val basicUserAnswersWithoutVatInfo: UserAnswers =
    UserAnswers(id = "12345-credId", vatInfo = None, lastUpdated = Instant.now())
      .set(IossNumberQuery, iossNumber).success.value


  private val ukBasedCompleteUserAnswersWithVatInfo: UserAnswers =
    basicUserAnswersWithVatInfo
      .set(BusinessBasedInUKPage, true).success.value
      .set(ClientHasVatNumberPage, true).success.value
      .set(ClientVatNumberPage, "GB123456").success.value
      .set(HasTradingNamePage, true).success.value
      .set(AllTradingNamesQuery, List(TradingName("Some Trading Name"))).success.value
      .set(PreviouslyRegisteredPage, true).success.value
      .set(AllPreviousRegistrationsQuery, List(previousRegistrationDetails)).success.value
      .set(HasFixedEstablishmentPage, true).success.value
      .set(AllEuDetailsQuery, List(arbitraryEuDetails.arbitrary.sample.value)).success.value
      .set(BusinessContactDetailsPage, businessContactDetails).success.value

  private val nonUkBasedCompleteUserAnswersWithVatInfo: UserAnswers =
    basicUserAnswersWithVatInfo
      .set(BusinessBasedInUKPage, false).success.value
      .set(ClientHasVatNumberPage, true).success.value
      .set(ClientVatNumberPage, "GB123456").success.value
      .set(ClientCountryBasedPage, arbitraryCountry.arbitrary.sample.value).success.value
      .set(HasTradingNamePage, true).success.value
      .set(AllTradingNamesQuery, List(TradingName("Some Trading Name"))).success.value
      .set(PreviouslyRegisteredPage, true).success.value
      .set(AllPreviousRegistrationsQuery, List(previousRegistrationDetails)).success.value
      .set(HasFixedEstablishmentPage, true).success.value
      .set(AllEuDetailsQuery, List(arbitraryEuDetails.arbitrary.sample.value)).success.value
      .set(BusinessContactDetailsPage, businessContactDetails).success.value

  private val ukBasedCompleteUserAnswersWithoutVatInfo: UserAnswers =
    basicUserAnswersWithoutVatInfo
      .set(BusinessBasedInUKPage, true).success.value
      .set(ClientHasUtrNumberPage, true).success.value
      .set(ClientUtrNumberPage, "UTR_NUM_1").success.value
      .set(HasTradingNamePage, true).success.value
      .set(AllTradingNamesQuery, List(TradingName("Some Trading Name"))).success.value
      .set(PreviouslyRegisteredPage, true).success.value
      .set(AllPreviousRegistrationsQuery, List(previousRegistrationDetails)).success.value
      .set(HasFixedEstablishmentPage, true).success.value
      .set(AllEuDetailsQuery, List(arbitraryEuDetails.arbitrary.sample.value)).success.value
      .set(BusinessContactDetailsPage, businessContactDetails).success.value
      .set(ClientBusinessNamePage, ClientBusinessName(companyName)).success.value

  private val nonUkBasedCompleteUserAnswersWithoutVatInfo: UserAnswers =
    basicUserAnswersWithoutVatInfo
      .set(BusinessBasedInUKPage, false).success.value
      .set(ClientCountryBasedPage, arbitraryCountry.arbitrary.sample.value).success.value
      .set(ClientTaxReferencePage, "FTR_NUM_1").success.value
      .set(HasTradingNamePage, true).success.value
      .set(AllTradingNamesQuery, List(TradingName("Some Trading Name"))).success.value
      .set(PreviouslyRegisteredPage, true).success.value
      .set(AllPreviousRegistrationsQuery, List(previousRegistrationDetails)).success.value
      .set(HasFixedEstablishmentPage, true).success.value
      .set(AllEuDetailsQuery, List(arbitraryEuDetails.arbitrary.sample.value)).success.value
      .set(BusinessContactDetailsPage, businessContactDetails).success.value
      .set(ClientBusinessNamePage, ClientBusinessName(companyName)).success.value

  private val mockRegistrationService: RegistrationService = mock[RegistrationService]

  "ChangeRegistration Controller" - {

    ".onPageLoad" - {

      "must return OK and the correct view for a GET when" - {

        "A NETP Has a Uk based address and has Vat Info" in {

          val application = applicationBuilder(userAnswers = Some(ukBasedCompleteUserAnswersWithVatInfo)).build()

          running(application) {

            val request = FakeRequest(GET, controllers.amend.routes.ChangeRegistrationController.onPageLoad().url)

            implicit val msgs: Messages = messages(application)

            val result = route(application, request).value

            val view = application.injector.instanceOf[ChangeRegistrationView]

            val registrationList = SummaryListViewModel(rows = getUkBasedWithVatNumRegistrationDetailsList(ukBasedCompleteUserAnswersWithVatInfo))

            val importOneStopShopDetailsList = SummaryListViewModel(
              rows = getImportOneStopShopDetailsSummaryList(ukBasedCompleteUserAnswersWithVatInfo, existingPreviousRegistrations)
            )

            status(result) mustBe OK
            contentAsString(result) mustBe
              view(
                waypoints,
                vatCustomerInfo.organisationName.get,
                iossNumber,
                registrationList,
                importOneStopShopDetailsList
              )(request, messages(application)).toString
          }
        }

        "A NETP Has a Uk based address and does NOT have Vat Info" in {

          val application = applicationBuilder(userAnswers = Some(ukBasedCompleteUserAnswersWithoutVatInfo)).build()

          running(application) {

            val request = FakeRequest(GET, controllers.amend.routes.ChangeRegistrationController.onPageLoad().url)

            implicit val msgs: Messages = messages(application)

            val result = route(application, request).value

            val view = application.injector.instanceOf[ChangeRegistrationView]

            val registrationList = SummaryListViewModel(rows = getUkBasedWithoutVatNumRegistrationDetailsList(ukBasedCompleteUserAnswersWithoutVatInfo))

            val importOneStopShopDetailsList = SummaryListViewModel(
              rows = getImportOneStopShopDetailsSummaryList(ukBasedCompleteUserAnswersWithoutVatInfo, existingPreviousRegistrations)
            )

            status(result) mustBe OK
            contentAsString(result) mustBe
              view(
                waypoints,
                companyName,
                iossNumber,
                registrationList,
                importOneStopShopDetailsList
              )(request, messages(application)).toString
          }
        }

        "A NETP Has a Non Uk based address and Vat Info" in {

          val application = applicationBuilder(userAnswers = Some(nonUkBasedCompleteUserAnswersWithVatInfo))
            .build()

          running(application) {
            val request = FakeRequest(GET, controllers.amend.routes.ChangeRegistrationController.onPageLoad().url)

            implicit val msgs: Messages = messages(application)

            val result = route(application, request).value

            val view = application.injector.instanceOf[ChangeRegistrationView]

            val registrationList = SummaryListViewModel(rows = getNonUkBasedWithVatNumRegistrationDetailsList(nonUkBasedCompleteUserAnswersWithVatInfo))

            val importOneStopShopDetailsList = SummaryListViewModel(
              rows = getImportOneStopShopDetailsSummaryList(nonUkBasedCompleteUserAnswersWithVatInfo, existingPreviousRegistrations)
            )


            status(result) mustBe OK
            contentAsString(result) mustBe view(
              waypoints,
              vatCustomerInfo.organisationName.get,
              iossNumber,
              registrationList,
              importOneStopShopDetailsList
            )(request, messages(application)).toString
          }
        }

        "A NETP Has a Non Uk based address does NOT have Vat Info" in {

          val application = applicationBuilder(userAnswers = Some(nonUkBasedCompleteUserAnswersWithoutVatInfo)).build()

          running(application) {

            val request = FakeRequest(GET, controllers.amend.routes.ChangeRegistrationController.onPageLoad().url)

            implicit val msgs: Messages = messages(application)

            val result = route(application, request).value

            val view = application.injector.instanceOf[ChangeRegistrationView]

            val registrationList = SummaryListViewModel(rows = getNonUkBasedWithoutVatNumRegistrationDetailsList(nonUkBasedCompleteUserAnswersWithoutVatInfo))

            val importOneStopShopDetailsList = SummaryListViewModel(
              rows = getImportOneStopShopDetailsSummaryList(nonUkBasedCompleteUserAnswersWithoutVatInfo, existingPreviousRegistrations)
            )

            status(result) mustBe OK
            contentAsString(result) mustBe
              view(
                waypoints,
                companyName,
                iossNumber,
                registrationList,
                importOneStopShopDetailsList
              )(request, messages(application)).toString
          }
        }
      }
    }

    ".onSubmit" - {

      "should trigger .amendRegistration and redirect to [to be implemented]" in {

        val etmpDisplayRegistration: EtmpDisplayRegistration = arbitraryRegistrationWrapper.arbitrary.sample.value.etmpDisplayRegistration
        val userAnswers = ukBasedCompleteUserAnswersWithVatInfo.set(OriginalRegistrationQuery(iossNumber), etmpDisplayRegistration).success.value
        val application = applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(bind[RegistrationService].toInstance(mockRegistrationService))
          .build()
        val amendRegistrationResponse: AmendRegistrationResponse = AmendRegistrationResponse(
          processingDateTime = LocalDateTime.now(),
          formBundleNumber = "123456789",
          intermediary = "IN900123456",
          businessPartner = "Test Business Partner"
        )
        when(mockRegistrationService.amendRegistration(any(), any(), any(), any())(any())) thenReturn Right(amendRegistrationResponse).toFuture
        running(application) {

          val request = FakeRequest(POST, controllers.amend.routes.ChangeRegistrationController.onSubmit().url)

          val result = route(application, request).value

          status(result) mustBe SEE_OTHER
          verify(mockRegistrationService, times(1)).amendRegistration(any(), any(), any(), any())(any())
        }
      }
    }

  }

  private def getUkBasedWithVatNumRegistrationDetailsList(answers: UserAnswers)(implicit msgs: Messages): Seq[SummaryListRow] = {

    Seq(
      BusinessBasedInUKSummary.rowWithoutAction(waypoints, answers),
      ClientHasVatNumberSummary.rowWithoutAction(waypoints, answers),
      ClientVatNumberSummary.rowWithoutAction(waypoints, answers),
      VatRegistrationDetailsSummary.changeRegBusinessAddressRow(waypoints, answers, amendYourAnswersPage)
    ).flatten
  }

  private def getNonUkBasedWithVatNumRegistrationDetailsList(answers: UserAnswers)(implicit msgs: Messages): Seq[SummaryListRow] = {

    Seq(
      BusinessBasedInUKSummary.rowWithoutAction(waypoints, answers),
      ClientHasVatNumberSummary.rowWithoutAction(waypoints, answers),
      ClientVatNumberSummary.rowWithoutAction(waypoints, answers),
      ClientCountryBasedSummary.row(waypoints, answers, amendYourAnswersPage),
      ClientBusinessNameSummary.row(waypoints, answers, amendYourAnswersPage),
      VatRegistrationDetailsSummary.changeRegVatBusinessNameRow(waypoints, answers, amendYourAnswersPage, false),
      VatRegistrationDetailsSummary.changeRegBusinessAddressRow(waypoints, answers, amendYourAnswersPage),
      ClientBusinessAddressSummary.row(waypoints, answers, amendYourAnswersPage)
    ).flatten
  }

  private def getUkBasedWithoutVatNumRegistrationDetailsList(answers: UserAnswers)(implicit msgs: Messages): Seq[SummaryListRow] = {

    Seq(
      BusinessBasedInUKSummary.rowWithoutAction(waypoints, answers),
      ClientHasVatNumberSummary.rowWithoutAction(waypoints, answers),
      ClientBusinessNameSummary.row(waypoints, answers, amendYourAnswersPage),
      ClientHasUtrNumberSummary.rowWithoutAction(waypoints, answers),
      ClientUtrNumberSummary.rowWithoutAction(waypoints, answers),
      ClientsNinoNumberSummary.row(waypoints, answers, amendYourAnswersPage),
      ClientBusinessAddressSummary.row(waypoints, answers, amendYourAnswersPage)
    ).flatten
  }

  private def getNonUkBasedWithoutVatNumRegistrationDetailsList(answers: UserAnswers)(implicit msgs: Messages): Seq[SummaryListRow] = {

    Seq(
      BusinessBasedInUKSummary.rowWithoutAction(waypoints, answers),
      ClientHasVatNumberSummary.rowWithoutAction(waypoints, answers),
      ClientCountryBasedSummary.row(waypoints, answers, amendYourAnswersPage),
      ClientTaxReferenceSummary.row(waypoints, answers, amendYourAnswersPage),
      ClientBusinessNameSummary.row(waypoints, answers, amendYourAnswersPage),
      ClientBusinessAddressSummary.row(waypoints, answers, amendYourAnswersPage)
    ).flatten
  }

  private def getImportOneStopShopDetailsSummaryList(answers: UserAnswers, previousRegistrations: Seq[PreviousRegistration])
                                                    (implicit msgs: Messages): Seq[SummaryListRow] = {
    val maybeHasTradingNameSummaryRow = HasTradingNameSummary.row(answers, waypoints, amendYourAnswersPage)
    val tradingNameSummaryRow = TradingNameSummary.checkAnswersRow(waypoints, answers, amendYourAnswersPage)
    val formattedHasTradingNameSummary = maybeHasTradingNameSummaryRow.map { nonOptHasTradingNameSummaryRow =>
      if (tradingNameSummaryRow.nonEmpty) {
        nonOptHasTradingNameSummaryRow.withCssClass("govuk-summary-list__row--no-border")
      } else {
        nonOptHasTradingNameSummaryRow
      }
    }

    val previouslyRegisteredSummaryRow = PreviouslyRegisteredSummary.rowWithoutAction(answers, waypoints)
    val previousRegistrationSummaryRow = PreviousRegistrationSummary.checkAnswersRow(answers, previousRegistrations, waypoints, amendYourAnswersPage)
    val formattedPreviouslyRegisteredSummaryRowy = previouslyRegisteredSummaryRow.map { nonOptPreviouslyRegisteredSummaryRow =>
      if (previousRegistrationSummaryRow.isDefined) {
        nonOptPreviouslyRegisteredSummaryRow.withCssClass("govuk-summary-list__row--no-border")
      } else {
        nonOptPreviouslyRegisteredSummaryRow
      }
    }
    val hasFixedEstablishmentSummaryRow = HasFixedEstablishmentSummary.row(waypoints, answers, amendYourAnswersPage)
    val euDetailsSummaryRow = EuDetailsSummary.checkAnswersRow(waypoints, answers, amendYourAnswersPage)
    val formattedHasFixedEstablishmentSummaryRow = hasFixedEstablishmentSummaryRow.map { nonOptHasFixedEstablishmentSummaryRow =>
      if (euDetailsSummaryRow.nonEmpty) {
        nonOptHasFixedEstablishmentSummaryRow.withCssClass("govuk-summary-list__row--no-border")
      } else {
        nonOptHasFixedEstablishmentSummaryRow.withCssClass("govuk-summary-list")
      }
    }
    val formattedContactName = BusinessContactDetailsSummary.rowFullName(waypoints, answers, amendYourAnswersPage).map(_.withCssClass("govuk-summary-list__row--no-border"))
    val formattedTelephoneNumber = BusinessContactDetailsSummary.rowTelephoneNumber(waypoints, answers, amendYourAnswersPage).map(_.withCssClass("govuk-summary-list__row--no-border"))
    val formattedEmailAddress = BusinessContactDetailsSummary.rowEmailAddress(waypoints, answers, amendYourAnswersPage)

    Seq(
      formattedHasTradingNameSummary,
      tradingNameSummaryRow,
      formattedPreviouslyRegisteredSummaryRowy,
      previousRegistrationSummaryRow,
      formattedHasFixedEstablishmentSummaryRow,
      euDetailsSummaryRow,
      WebsiteSummary.checkAnswersRow(waypoints, answers, amendYourAnswersPage),
      formattedContactName,
      formattedTelephoneNumber,
      formattedEmailAddress
    ).flatten
  }
}

