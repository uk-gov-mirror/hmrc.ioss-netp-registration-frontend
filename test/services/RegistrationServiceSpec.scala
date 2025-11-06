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

package services

import base.SpecBase
import connectors.RegistrationConnector
import models.domain.{PreviousRegistration, PreviousSchemeDetails, VatCustomerInfo}
import models.etmp.*
import models.etmp.EtmpIdType.{FTR, NINO, UTR, VRN}
import models.etmp.display.*
import models.responses.UnexpectedResponseStatus
import models.responses.etmp.EtmpEnrolmentResponse
import models.vatEuDetails.{EuDetails, RegistrationType, TradingNameAndBusinessAddress}
import models.{BusinessContactDetails, ClientBusinessName, Country, InternationalAddress, TradingName, UserAnswers, Website}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{reset, times, verify, when}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.BeforeAndAfterEach
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatestplus.mockito.MockitoSugar.mock
import pages.previousRegistrations.PreviouslyRegisteredPage
import pages.tradingNames.HasTradingNamePage
import pages.vatEuDetails.HasFixedEstablishmentPage
import pages.{BusinessBasedInUKPage, BusinessContactDetailsPage, ClientBusinessAddressPage, ClientBusinessNamePage, ClientCountryBasedPage, ClientHasUtrNumberPage, ClientHasVatNumberPage, ClientTaxReferencePage, ClientUtrNumberPage, ClientVatNumberPage, ClientsNinoNumberPage}
import play.api.test.Helpers.running
import queries.AllWebsites
import queries.euDetails.AllEuDetailsQuery
import queries.previousRegistrations.AllPreviousRegistrationsQuery
import queries.tradingNames.AllTradingNamesQuery
import testutils.{RegistrationData, WireMockHelper}
import uk.gov.hmrc.http.HeaderCarrier
import utils.CheckUkBased.isUkBasedNetp
import utils.FutureSyntax.FutureOps


class RegistrationServiceSpec extends SpecBase with WireMockHelper with BeforeAndAfterEach with TableDrivenPropertyChecks {

  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private val mockRegistrationConnector: RegistrationConnector = mock[RegistrationConnector]
  private val registrationService = new RegistrationService(stubClockAtArbitraryDate, mockRegistrationConnector)

  private val registrationWrapper: RegistrationWrapper = arbitraryRegistrationWrapper.arbitrary.sample.value
  private val etmpOtherAddress: EtmpOtherAddress = arbitraryEtmpOtherAddress.arbitrary.sample.value

  override def beforeEach(): Unit = {
    reset(mockRegistrationConnector)
  }

  ".createRegistration" - {

    "must create a registration request from user answers provided and return a successful ETMP enrolment response" in {

      val etmpEnrolmentResponse: EtmpEnrolmentResponse =
        EtmpEnrolmentResponse(iossReference = arbitrary[TaxRefTraderID].sample.value.taxReferenceNumber)

      when(mockRegistrationConnector.createRegistration(any())(any())) thenReturn Right(etmpEnrolmentResponse).toFuture

      val app = applicationBuilder(Some(basicUserAnswersWithVatInfo), Some(stubClockAtArbitraryDate))
        .build()

      running(app) {

        registrationService.createRegistration(basicUserAnswersWithVatInfo).futureValue mustBe Right(etmpEnrolmentResponse)
        verify(mockRegistrationConnector, times(1)).createRegistration(any())(any())
      }
    }
  }

  ".amendRegistration" - {

    "must return a successful AmendRegistrationResponse when connector succeeds" in {

      val amendResponse = RegistrationData.amendRegistrationResponse
      val registration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value

      when(mockRegistrationConnector.amendRegistration(any())(any())) thenReturn Right(amendResponse).toFuture

      val app = applicationBuilder(Some(basicUserAnswersWithVatInfo), Some(stubClockAtArbitraryDate))
        .build()

      running(app) {

        registrationService.amendRegistration(
          answers = basicUserAnswersWithVatInfo,
          registration = registration,
          iossNumber = intermediaryNumber,
          rejoin = false
        ).futureValue mustBe Right(amendResponse)

        verify(mockRegistrationConnector, times(1)).amendRegistration(any())(any())
      }
    }

    "must return error when connector fails" in {
      val registration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value
      val error = UnexpectedResponseStatus(500, "Server error")

      when(mockRegistrationConnector.amendRegistration(any())(any())) thenReturn Left(error).toFuture

      val app = applicationBuilder(Some(basicUserAnswersWithVatInfo)).build()

      running(app) {
        registrationService.amendRegistration(
          answers = basicUserAnswersWithVatInfo,
          registration = registration,
          iossNumber = intermediaryNumber
        ).futureValue mustBe Left(error)
      }
    }
  }

  ".toUserAnswers" - {

    "must convert from RegistrationWrapper to UserAnswers" - {

      "when client is UK based with Vat Info" in {

        val ukCountryCode: String = "GB"

        val ukRegistrationWrapper: RegistrationWrapper = registrationWrapper
          .copy(vatInfo = Some(registrationWrapper.vatInfo.get.
            copy(desAddress = registrationWrapper.vatInfo.get.desAddress
              .copy(countryCode = ukCountryCode)
            )),
            etmpDisplayRegistration = registrationWrapper.etmpDisplayRegistration
              .copy(
                customerIdentification = EtmpDisplayCustomerIdentification(VRN, registrationWrapper.etmpDisplayRegistration.customerIdentification.idValue),
                otherAddress = None
              )
          )

        val service = new RegistrationService(stubClockAtArbitraryDate, mockRegistrationConnector)

        val result = service.toUserAnswers(userAnswersId, ukRegistrationWrapper).futureValue

        result `mustBe` convertedUserAnswers(ukRegistrationWrapper).copy(lastUpdated = result.lastUpdated, journeyId = result.journeyId)
      }

      "when client is UK based without Vat Info" in {

        val ukCountryCode: String = "GB"

        val ukRegistrationWrapper: RegistrationWrapper = registrationWrapper
          .copy(vatInfo = None,
            etmpDisplayRegistration = registrationWrapper.etmpDisplayRegistration
              .copy(
                customerIdentification = EtmpDisplayCustomerIdentification(UTR, registrationWrapper.etmpDisplayRegistration.customerIdentification.idValue),
                otherAddress = Some(etmpOtherAddress.copy(issuedBy = ukCountryCode))
              )
          )

        val service = new RegistrationService(stubClockAtArbitraryDate, mockRegistrationConnector)

        val result = service.toUserAnswers(userAnswersId, ukRegistrationWrapper).futureValue

        result `mustBe` convertedUserAnswers(ukRegistrationWrapper).copy(lastUpdated = result.lastUpdated, journeyId = result.journeyId)
      }

      "when user is NOT based in UK but has a Vat Number" in {

        val nonUkCountryCode: String = "DE"

        val nonUkRegistrationWrapper = registrationWrapper.copy(
          vatInfo = Some(registrationWrapper.vatInfo.get.
            copy(desAddress = registrationWrapper.vatInfo.get.desAddress
              .copy(countryCode = nonUkCountryCode)
            )),
          etmpDisplayRegistration = registrationWrapper.etmpDisplayRegistration
            .copy(
              customerIdentification = EtmpDisplayCustomerIdentification(VRN, registrationWrapper.etmpDisplayRegistration.customerIdentification.idValue),
              otherAddress = None
            )
        )

        val service = new RegistrationService(stubClockAtArbitraryDate, mockRegistrationConnector)

        val result = service.toUserAnswers(userAnswersId, nonUkRegistrationWrapper).futureValue

        result `mustBe` convertedUserAnswers(nonUkRegistrationWrapper).copy(lastUpdated = result.lastUpdated, journeyId = result.journeyId)
      }

      "when user is NOT based in UK and does not have a Vat Number" in {

        val nonUkCountryCode: String = "DE"

        val nonUkRegistrationWrapper = registrationWrapper.copy(
          vatInfo = None,
          etmpDisplayRegistration = registrationWrapper.etmpDisplayRegistration
            .copy(
              customerIdentification = EtmpDisplayCustomerIdentification(UTR, registrationWrapper.etmpDisplayRegistration.customerIdentification.idValue),
              otherAddress = Some(etmpOtherAddress.copy(issuedBy = nonUkCountryCode))
            )
        )

        val service = new RegistrationService(stubClockAtArbitraryDate, mockRegistrationConnector)

        val result = service.toUserAnswers(userAnswersId, nonUkRegistrationWrapper).futureValue

        result `mustBe` convertedUserAnswers(nonUkRegistrationWrapper).copy(lastUpdated = result.lastUpdated, journeyId = result.journeyId)
      }

    }

    "must throw an Illegal State Exception when non-UK based client does not provide their address details" in {

      val nonUkRegistrationWrapper = registrationWrapper.copy(
        vatInfo = None,
        etmpDisplayRegistration = registrationWrapper.etmpDisplayRegistration
          .copy(otherAddress = None)
      )

      val service = new RegistrationService(stubClockAtArbitraryDate, mockRegistrationConnector)

      val exception = intercept[IllegalStateException] {
        service.toUserAnswers(userAnswersId, nonUkRegistrationWrapper).failed
      }
      exception.getMessage mustBe
        "Unable to identify if client is based in the UK. Client requires either Vat Customer Info or an Etmp Other Address from ETMP for amend journey."
    }

    "must throw an Illegal State Exception when country doesn't exist" in {

      val nonExistentCountryCode: String = "non-existent"

      val nonUkRegistrationWrapper = registrationWrapper.copy(
        vatInfo = Some(registrationWrapper.vatInfo.get.
          copy(desAddress = registrationWrapper.vatInfo.get.desAddress
            .copy(countryCode = nonExistentCountryCode)
          )),
        etmpDisplayRegistration = registrationWrapper.etmpDisplayRegistration
          .copy(otherAddress = registrationWrapper.etmpDisplayRegistration.otherAddress.map(_.copy(issuedBy = nonExistentCountryCode)))
      )

      val service = new RegistrationService(stubClockAtArbitraryDate, mockRegistrationConnector)

      val result = service.toUserAnswers(userAnswersId, nonUkRegistrationWrapper).failed

      whenReady(result) { exp =>
        exp mustBe a[IllegalStateException]
        exp.getMessage mustBe
          s"Unable to find country $nonExistentCountryCode"
      }
    }
  }

  ".setClientCountry" - {
    val nonUkCountryCode: String = "DE"
    val ukCountryCode: String = "GB"
    val nonUkVatInfo: VatCustomerInfo = vatCustomerInfo.copy(desAddress = vatCustomerInfo.desAddress.copy(countryCode = nonUkCountryCode))
    val ukVatInfo: VatCustomerInfo = vatCustomerInfo.copy(desAddress = vatCustomerInfo.desAddress.copy(countryCode = ukCountryCode))
    val nonUkVatAnswers = emptyUserAnswers.copy(vatInfo = Some(nonUkVatInfo))
    val ukVatAnswers = emptyUserAnswers.copy(vatInfo = Some(ukVatInfo))
    val noVatUserAnswers = emptyUserAnswers.copy(vatInfo = None)
    val nonUkOtherAddress = arbitraryEtmpOtherAddress.arbitrary.sample.value.copy(issuedBy = nonUkCountryCode)

    "When user has EtmpOtherAddress set ClientCountryBasedPage as this country regardless of Vat Address" in {

      val etmpTestCases = Table(
        ("userAnswers", "EtmpOtherAddress"),
        (ukVatAnswers, Some(nonUkOtherAddress)),
        (nonUkVatAnswers, Some(nonUkOtherAddress)),
        (noVatUserAnswers, Some(nonUkOtherAddress))
      )

      forAll(etmpTestCases) { (userAnswers, etmpOtherAddress) =>
        val result: UserAnswers = registrationService
          .setClientCountry(userAnswers = userAnswers, optionEtmpOtherAddress = etmpOtherAddress).success.value

        val expectedUserAnswers: UserAnswers = userAnswers
          .set(ClientCountryBasedPage, Country("DE", "Germany")).success.value

        result mustBe expectedUserAnswers
      }
    }

    "When user does NOT have Some(EtmpOtherAddress) set ClientCountryBasedPage to country from the Vat address" in {

      val result: UserAnswers = registrationService
        .setClientCountry(userAnswers = ukVatAnswers, optionEtmpOtherAddress = None).success.value

      val expectedUserAnswers: UserAnswers = ukVatAnswers
        .set(ClientCountryBasedPage, Country("GB", "United Kingdom")).success.value

      result mustBe expectedUserAnswers
    }

    "When neither value is available for address return userAnswers unchanged" in {

      val result: UserAnswers = registrationService
        .setClientCountry(userAnswers = noVatUserAnswers, optionEtmpOtherAddress = None).success.value

      val expectedUserAnswers: UserAnswers = noVatUserAnswers

      result mustBe expectedUserAnswers
    }
  }

  ".setNonVatAddressDetails" - {
    val nonUkCountryCode: String = "DE"
    val etmpOtherAddress: EtmpOtherAddress = arbitraryEtmpOtherAddress.arbitrary.sample.value.copy(issuedBy = nonUkCountryCode)
    "when EtmpOtherAddress is available return userAnswers with businessAddress updated" in {

      val result = registrationService
        .setNonVatAddressDetails(userAnswers = emptyUserAnswers, maybeOtherAddress = Some(etmpOtherAddress)).success.value

      val expectedUserAnswers: UserAnswers = emptyUserAnswers
        .set(ClientBusinessAddressPage, convertNonUkAddress(Some(etmpOtherAddress))).success.value
        .set(ClientBusinessNamePage, ClientBusinessName(etmpOtherAddress.tradingName.get)).success.value

      result mustBe expectedUserAnswers
    }

    "when EtmpOtherAddress is not present return userAnswers unchanged" in {

      val result: UserAnswers = registrationService
        .setNonVatAddressDetails(userAnswers = emptyUserAnswers, maybeOtherAddress = None).success.value

      val expectedUserAnswers: UserAnswers = emptyUserAnswers

      result mustBe expectedUserAnswers
    }

  }

  ".getTaxIdentifierAndNum" - {
    val vatCustomerId: EtmpDisplayCustomerIdentification = EtmpDisplayCustomerIdentification(VRN, "101")
    val utrCustomerId: EtmpDisplayCustomerIdentification = EtmpDisplayCustomerIdentification(UTR, "101")
    val ftrCustomerId: EtmpDisplayCustomerIdentification = EtmpDisplayCustomerIdentification(FTR, "101")
    val ninoCustomerId: EtmpDisplayCustomerIdentification = EtmpDisplayCustomerIdentification(NINO, "101")

    "when customer id type is VRN set ClientHasVatNumberPage true and ClientVatNumberPage as the id value" in {

      val result = registrationService
        .getTaxIdentifierAndNum(userAnswers = emptyUserAnswers, customerInfo = vatCustomerId).success.value

      val expectedUserAnswers = emptyUserAnswers
        .set(ClientVatNumberPage, vatCustomerId.idValue).success.value

      result mustBe expectedUserAnswers
    }
    "when customer id type is UTR set ClientHasUtrNumberPage true and ClientUtrNumberPage as the id value" in {
      val result = registrationService
        .getTaxIdentifierAndNum(userAnswers = emptyUserAnswers, customerInfo = utrCustomerId).success.value

      val expectedUserAnswers = emptyUserAnswers
        .set(ClientHasUtrNumberPage, true).success.value
        .set(ClientUtrNumberPage, utrCustomerId.idValue).success.value

      result mustBe expectedUserAnswers
    }
    "when customer id type is NINO set ClientHasUtrNumberPage false and ClientsNinoNumberPage as the id value" in {
      val result = registrationService
        .getTaxIdentifierAndNum(userAnswers = emptyUserAnswers, customerInfo = ninoCustomerId).success.value

      val expectedUserAnswers = emptyUserAnswers
        .set(ClientHasUtrNumberPage, false).success.value
        .set(ClientsNinoNumberPage, ninoCustomerId.idValue).success.value

      result mustBe expectedUserAnswers
    }
    "when customer id type is FTR set ClientTaxReferencePage as the id value" in {
      val result = registrationService
        .getTaxIdentifierAndNum(userAnswers = emptyUserAnswers, customerInfo = ftrCustomerId).success.value

      val expectedUserAnswers = emptyUserAnswers
        .set(ClientTaxReferencePage, ftrCustomerId.idValue).success.value

      result mustBe expectedUserAnswers
    }
  }

  private def convertedUserAnswers(registrationWrapper: RegistrationWrapper): UserAnswers = {

    val displayRegistration: EtmpDisplayRegistration = registrationWrapper.etmpDisplayRegistration
    val convertedTradingNamesUA: Seq[TradingName] = convertTradingNames(displayRegistration.tradingNames)
    val convertedPreviousEuRegistrationDetails: Seq[PreviousRegistration] =
      convertEtmpPreviousEuRegistrations(displayRegistration.schemeDetails.previousEURegistrationDetails)
    val convertedEuFixedEstablishmentDetails: Seq[EuDetails] =
      convertEuFixedEstablishmentDetails(displayRegistration.schemeDetails.euRegistrationDetails)
    val contactDetails: BusinessContactDetails = getContactDetails(displayRegistration.schemeDetails)
    val isUkBased = isUkBasedNetp(registrationWrapper.vatInfo, registrationWrapper.etmpDisplayRegistration.otherAddress)

    val businessBasedUkUA: UserAnswers = emptyUserAnswers
      .set(BusinessBasedInUKPage, isUkBased).success.value

    val userAnswersWithVat = if (registrationWrapper.vatInfo.isDefined) {
      businessBasedUkUA.copy(vatInfo = Some(registrationWrapper.vatInfo.get))
        .set(ClientHasVatNumberPage, true).success.value
        .set(ClientVatNumberPage, registrationWrapper.etmpDisplayRegistration.customerIdentification.idValue).success.value
    } else {
      businessBasedUkUA
        .copy(vatInfo = None)
        .set(ClientHasUtrNumberPage, true).success.value
        .set(ClientUtrNumberPage, registrationWrapper.etmpDisplayRegistration.customerIdentification.idValue).success.value
    }

    val addBusinessDetailsUA: UserAnswers = if (registrationWrapper.vatInfo.isEmpty) {
      userAnswersWithVat
        .set(ClientBusinessAddressPage, convertNonUkAddress(displayRegistration.otherAddress)).success.value
        .set(ClientBusinessNamePage, ClientBusinessName(displayRegistration.otherAddress.get.tradingName.get)).success.value
    } else {
      userAnswersWithVat
    }

    val userAnswersPreCountry: UserAnswers = addBusinessDetailsUA
      .set(HasTradingNamePage, convertedTradingNamesUA.nonEmpty).success.value
      .set(AllTradingNamesQuery, convertedTradingNamesUA.toList).success.value
      .set(PreviouslyRegisteredPage, convertedPreviousEuRegistrationDetails.nonEmpty).success.value
      .set(AllPreviousRegistrationsQuery, convertedPreviousEuRegistrationDetails.toList).success.value
      .set(HasFixedEstablishmentPage, convertedEuFixedEstablishmentDetails.nonEmpty).success.value
      .set(AllEuDetailsQuery, convertedEuFixedEstablishmentDetails.toList).success.value
      .set(BusinessContactDetailsPage, contactDetails).success.value
      .set(AllWebsites, convertWebsite(registrationWrapper.etmpDisplayRegistration.schemeDetails.websites)).success.value


    if (registrationWrapper.etmpDisplayRegistration.otherAddress.isDefined) {
      userAnswersPreCountry.set(ClientCountryBasedPage, getCountry(registrationWrapper.etmpDisplayRegistration.otherAddress.get.issuedBy)).success.value
    } else {
      userAnswersPreCountry.set(ClientCountryBasedPage, getCountry(registrationWrapper.vatInfo.get.desAddress.countryCode)).success.value
    }
  }

  private def convertTradingNames(etmpTradingNames: Seq[EtmpTradingName]): Seq[TradingName] = {
    for {
      etmpTradingName <- etmpTradingNames
    } yield TradingName(name = etmpTradingName.tradingName)
  }

  private def convertWebsite(etmpWebsites: Seq[EtmpWebsite]): List[Website] = {
    etmpWebsites.map(etmpWebsite => Website(etmpWebsite.websiteAddress)).toList
  }

  private def convertEtmpPreviousEuRegistrations(allEtmpPreviousEuRegistrationDetails: Seq[EtmpPreviousEuRegistrationDetails]): List[PreviousRegistration] = {
    val countrySchemaDetailsMapping: Map[Country, Seq[(Country, PreviousSchemeDetails)]] =
      allEtmpPreviousEuRegistrationDetails.map { etmpPreviousEuRegistrationDetails =>
        val country = Country.fromCountryCodeUnsafe(etmpPreviousEuRegistrationDetails.issuedBy)
        val details: PreviousSchemeDetails = PreviousSchemeDetails.fromEtmpPreviousEuRegistrationDetails(etmpPreviousEuRegistrationDetails)

        country -> details

      }.groupBy(_._1)

    countrySchemaDetailsMapping.map { case (country, countryPreviousSchemaDetails) =>
      PreviousRegistration(previousEuCountry = country, previousSchemesDetails = countryPreviousSchemaDetails.map(_._2))
    }.toList
  }

  private def getCountry(countryCode: String): Country = {
    Country.fromCountryCodeAllCountries(countryCode) match {
      case Some(country) => country
      case _ =>
        val exception = new IllegalStateException(s"Unable to find country $countryCode")
        throw exception
    }
  }

  private def determineRegistrationType(vatNumber: Option[String], taxIdentificationNumber: Option[String]): Option[RegistrationType] = {
    (vatNumber, taxIdentificationNumber) match {
      case (Some(_), _) => Some(RegistrationType.VatNumber)
      case _ => Some(RegistrationType.TaxId)
    }
  }

  private def convertEuVatNumber(countryCode: String, maybeVatNumber: Option[String]): Option[String] = {
    maybeVatNumber.map { vatNumber =>
      s"$countryCode$vatNumber"
    }
  }

  private def convertEuFixedEstablishmentDetails(etmpEuRegistrationDetails: Seq[EtmpDisplayEuRegistrationDetails]): Seq[EuDetails] = {
    for {
      etmpDisplayEuRegistrationDetails <- etmpEuRegistrationDetails
    } yield {
      EuDetails(
        euCountry = getCountry(etmpDisplayEuRegistrationDetails.issuedBy),
        hasFixedEstablishment = Some(true),
        registrationType = determineRegistrationType(
          etmpDisplayEuRegistrationDetails.vatNumber,
          etmpDisplayEuRegistrationDetails.taxIdentificationNumber
        ),
        euVatNumber = convertEuVatNumber(etmpDisplayEuRegistrationDetails.issuedBy, etmpDisplayEuRegistrationDetails.vatNumber),
        euTaxReference = etmpDisplayEuRegistrationDetails.taxIdentificationNumber,
        tradingNameAndBusinessAddress = Some(TradingNameAndBusinessAddress(
          tradingName = TradingName(etmpDisplayEuRegistrationDetails.fixedEstablishmentTradingName),
          address = InternationalAddress(
            line1 = etmpDisplayEuRegistrationDetails.fixedEstablishmentAddressLine1,
            line2 = etmpDisplayEuRegistrationDetails.fixedEstablishmentAddressLine2,
            townOrCity = etmpDisplayEuRegistrationDetails.townOrCity,
            stateOrRegion = etmpDisplayEuRegistrationDetails.regionOrState,
            postCode = etmpDisplayEuRegistrationDetails.postcode,
            country = Some(getCountry(etmpDisplayEuRegistrationDetails.issuedBy))
          )
        ))
      )
    }
  }

  private def getContactDetails(schemeDetails: EtmpDisplaySchemeDetails): BusinessContactDetails = {
    BusinessContactDetails(
      fullName = schemeDetails.contactName,
      telephoneNumber = schemeDetails.businessTelephoneNumber,
      emailAddress = schemeDetails.businessEmailId
    )
  }

  private def convertNonUkAddress(maybeOtherAddress: Option[EtmpOtherAddress]): InternationalAddress = {
    maybeOtherAddress.map { otherAddress =>
      InternationalAddress(
        line1 = otherAddress.addressLine1,
        line2 = otherAddress.addressLine2,
        townOrCity = otherAddress.townOrCity,
        stateOrRegion = otherAddress.regionOrState,
        postCode = otherAddress.postcode,
        country = Some(getCountry(otherAddress.issuedBy))
      )
    }.getOrElse {
      val exception = new IllegalStateException(s"Must have A UK Address.")
      throw exception
    }
  }
}