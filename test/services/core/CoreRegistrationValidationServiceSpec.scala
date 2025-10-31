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

package services.core

import base.SpecBase
import connectors.core.ValidateCoreRegistrationConnector
import models.audit.CoreRegistrationAuditModel
import models.core.*
import models.iossRegistration.*
import models.ossRegistration.*
import models.requests.DataRequest
import models.responses.UnexpectedResponseStatus
import models.{BankDetails, Bic, Country, Iban, PreviousScheme}
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito
import org.mockito.Mockito.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.mockito.MockitoSugar.mock
import play.api.http.Status.{BAD_REQUEST, INTERNAL_SERVER_ERROR, NOT_FOUND}
import play.api.mvc.AnyContent
import play.api.test.FakeRequest
import services.AuditService
import services.ioss.IossRegistrationService
import services.oss.OssRegistrationService
import uk.gov.hmrc.domain.Vrn
import uk.gov.hmrc.http.HeaderCarrier
import utils.FutureSyntax.FutureOps

import java.time.{Instant, LocalDate}
import scala.concurrent.ExecutionContext.Implicits.global

class CoreRegistrationValidationServiceSpec extends SpecBase with MockitoSugar with ScalaFutures with Matchers with BeforeAndAfterEach {

  private val genericMatch = Match(
    TraderId("333333333"),
    None,
    "EE",
    Some(2),
    None,
    None,
    None,
    None
  )

  private val coreValidationResponses: CoreRegistrationValidationResult =
    CoreRegistrationValidationResult(
      "333333333",
      None,
      "EE",
      traderFound = true,
      Seq(
        genericMatch
      ))

  private val mockValidateCoreRegistrationConnector: ValidateCoreRegistrationConnector = mock[ValidateCoreRegistrationConnector]
  private val iossRegistrationService: IossRegistrationService = mock[IossRegistrationService]
  private val ossRegistrationService: OssRegistrationService = mock[OssRegistrationService]
  private val mockAuditService: AuditService = mock[AuditService]

  implicit val hc: HeaderCarrier = HeaderCarrier()

  private val request = DataRequest(FakeRequest("GET", "/"), vrn.vrn, emptyUserAnswers, intermediaryDetails.intermediaryNumber, None)

  implicit private val dataRequest: DataRequest[AnyContent] =
    DataRequest(request, vrn.vrn, emptyUserAnswers, intermediaryDetails.intermediaryNumber, None)

  private def baseCoreRegistrationRequest(
                                           source: String,
                                           scheme: Option[String] = None,
                                           searchId: String = "333333333",
                                           searchIntermediary: Option[String] = None,
                                           searchIdIssuedBy: String
                                         ): CoreRegistrationRequest = CoreRegistrationRequest(
    source = source,
    scheme = scheme,
    searchId = searchId,
    searchIntermediary = searchIntermediary,
    searchIdIssuedBy = searchIdIssuedBy
  )

  override def beforeEach(): Unit = {
    Mockito.reset(
      mockValidateCoreRegistrationConnector,
      mockAuditService
    )
  }

  "coreRegistrationValidationService.searchUkVrn" - {

    val vrn: Vrn = Vrn("333333333")
    val countryCode: String = "GB"

    "must audit the event" - {

      val coreRegistrationRequest: CoreRegistrationRequest = baseCoreRegistrationRequest(
        source = "VATNumber",
        searchIdIssuedBy = countryCode
      )

      "and return match data for any matchType" in {

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(coreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector,
          iossRegistrationService,
          ossRegistrationService,
          mockAuditService
        )

        val value = coreRegistrationValidationService.searchUkVrn(vrn).futureValue.get

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, coreValidationResponses
        )

        value `mustBe` genericMatch
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }

      "and return None when no active match found" in {

        val expectedResponse = coreValidationResponses.copy(matches = Seq[Match]())
        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(expectedResponse).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val value = coreRegistrationValidationService.searchUkVrn(vrn).futureValue

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, expectedResponse
        )

        value `mustBe` None
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }
    }

    "must return exception when server responds with an error" in {

      val errorCode = Gen.oneOf(BAD_REQUEST, NOT_FOUND, INTERNAL_SERVER_ERROR).sample.value

      when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Left(UnexpectedResponseStatus(errorCode, "error")).toFuture

      val coreRegistrationValidationService = new CoreRegistrationValidationService(
        mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
      )

      val response = intercept[Exception](coreRegistrationValidationService.searchUkVrn(vrn).futureValue)

      response.getMessage must include("Error while validating core registration")
      verifyNoInteractions(mockAuditService)
    }
  }

  "coreRegistrationValidationService.searchEuTaxId" - {

    val taxRefNo: String = "333333333"
    val countryCode: String = "DE"

    "must audit the event" - {

      val coreRegistrationRequest: CoreRegistrationRequest = baseCoreRegistrationRequest(
        source = "EUTraderId",
        searchIdIssuedBy = countryCode
      )

      "and return match data for the a Tax reference number" in {

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(coreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val value = coreRegistrationValidationService.searchEuTaxId(taxRefNo, countryCode).futureValue.get

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, coreValidationResponses
        )

        value `mustBe` genericMatch
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }

      "and return None when no match found" in {

        val expectedResponse = coreValidationResponses.copy(matches = Seq[Match]())
        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(expectedResponse).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val value = coreRegistrationValidationService.searchEuTaxId(taxRefNo, countryCode).futureValue

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, expectedResponse
        )

        value `mustBe` None
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }
    }

    "must return exception when server responds with an error" in {

      val errorCode = Gen.oneOf(BAD_REQUEST, NOT_FOUND, INTERNAL_SERVER_ERROR).sample.value

      when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Left(UnexpectedResponseStatus(errorCode, "error")).toFuture

      val coreRegistrationValidationService = new CoreRegistrationValidationService(
        mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
      )

      val response = intercept[Exception](coreRegistrationValidationService.searchEuTaxId(taxRefNo, countryCode).futureValue)

      response.getMessage must include("Error while validating core registration")
      verifyNoInteractions(mockAuditService)
    }
  }

  "coreRegistrationValidationService.searchEuVrn" - {

    val euVrn: String = "333333333"
    val countryCode: String = "DE"

    "must audit the event" - {

      val coreRegistrationRequest = baseCoreRegistrationRequest(
        source = "EUTraderId",
        searchIdIssuedBy = countryCode
      )

      "and return match data for a EU VRN" in {

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(coreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val value = coreRegistrationValidationService.searchEuVrn(euVrn, countryCode).futureValue.get

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, coreValidationResponses
        )

        value `mustBe` genericMatch
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }

      "and return None when no match found" in {

        val expectedResponse = coreValidationResponses.copy(matches = Seq[Match]())
        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(expectedResponse).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val value = coreRegistrationValidationService.searchEuVrn(euVrn, countryCode).futureValue

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, expectedResponse
        )

        value `mustBe` None
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }
    }

    "must return exception when server responds with an error" in {

      val errorCode = Gen.oneOf(BAD_REQUEST, NOT_FOUND, INTERNAL_SERVER_ERROR).sample.value

      when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Left(UnexpectedResponseStatus(errorCode, "error")).toFuture

      val coreRegistrationValidationService = new CoreRegistrationValidationService(
        mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
      )

      val response = intercept[Exception](coreRegistrationValidationService.searchEuVrn(euVrn, countryCode).futureValue)

      response.getMessage must include("Error while validating core registration")
      verifyNoInteractions(mockAuditService)
    }
  }

  "coreRegistrationValidationService.searchScheme" - {

    val iossNumber: String = "333333333"
    val countryCode: String = "DE"

    "must audit the event" - {

      val coreRegistrationRequest = baseCoreRegistrationRequest(
        source = "EUTraderId",
        scheme = Some("OSS"),
        searchIdIssuedBy = countryCode
      )

      "and return match data for an ioss number" in {

        val previousScheme: PreviousScheme = PreviousScheme.OSSU

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(coreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val value = coreRegistrationValidationService.searchScheme(iossNumber, previousScheme, None, countryCode).futureValue.get

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, coreValidationResponses
        )

        value `mustBe` genericMatch
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }

      "and return match data for an ioss number with an intermediary" in {

        val iossNumber: String = "IM333222111"
        val intermediaryNumber: String = "IN555444222"
        val previousScheme: PreviousScheme = PreviousScheme.OSSU

        val coreRegistrationRequest = baseCoreRegistrationRequest(
          source = "EUTraderId",
          scheme = Some("OSS"),
          searchId = iossNumber,
          searchIntermediary = Some(intermediaryNumber),
          searchIdIssuedBy = countryCode
        )

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(coreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val value = coreRegistrationValidationService.searchScheme(iossNumber, previousScheme, Some(intermediaryNumber), countryCode).futureValue.get

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, coreValidationResponses
        )

        value `mustBe` genericMatch
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }

      "and return None when no match found" in {

        val previousScheme: PreviousScheme = PreviousScheme.OSSU

        val expectedResponse = coreValidationResponses.copy(matches = Seq[Match]())
        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(expectedResponse).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val coreRegistrationValidationService = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val value = coreRegistrationValidationService.searchScheme(iossNumber, previousScheme, None, countryCode).futureValue

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, expectedResponse
        )

        value `mustBe` None
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }
    }

    "must return IOSS Match when countryCode is XI and previousScheme is IOSS" in {

      val iossNumber = "IM900123456789"
      val countryCode = Country.northernIreland.code
      val previousScheme = PreviousScheme.IOSSWOI

      val exclusion = IossEtmpExclusion(
        exclusionReason = IossEtmpExclusionReason.FailsToComply,
        decisionDate = LocalDate.of(2022, 1, 1),
        effectiveDate = LocalDate.of(2022, 2, 1),
        quarantine = true
      )

      val iossDisplayRegistration = IossEtmpDisplayRegistration(
        tradingNames = Seq(IossEtmpTradingName("test 1")),
        schemeDetails = IossEtmpDisplaySchemeDetails(
          contactName = "Test Trader",
          businessTelephoneNumber = "123456",
          businessEmailId = "test@example.com"
        ),
        bankDetails = IossEtmpBankDetails(
          accountName = "Test Account",
          iban = Iban("GB33BUKB20201555555555").value,
          bic = Some(Bic("ABCDGB2A").get)
        ),
        exclusions = Seq(exclusion)
      )

      val expectedResponse: CoreRegistrationValidationResult = coreValidationResponses.copy(
        searchId = iossNumber,
        searchIdIssuedBy = countryCode,
        matches = Seq.empty
      )

      when(iossRegistrationService.getIossRegistration(iossNumber)) thenReturn iossDisplayRegistration.toFuture
      when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(expectedResponse).toFuture

      val expectedMatch = Match(
        traderId = TraderId(iossNumber),
        intermediary = None,
        memberState = countryCode,
        exclusionStatusCode = Some(4),
        exclusionDecisionDate = Some("2022-01-01"),
        exclusionEffectiveDate = Some("2022-02-01"),
        nonCompliantReturns = None,
        nonCompliantPayments = None
      )

      val service = new CoreRegistrationValidationService(
        mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
      )

      val result = service.searchScheme(iossNumber, previousScheme, None, countryCode).futureValue

      result `mustBe` Some(expectedMatch)
      verifyNoInteractions(mockAuditService)
    }

    "must return OSS Match when countryCode is XI and previousScheme is OSS" in {

      val vrn = "600000014"
      val countryCode = Country.northernIreland.code
      val previousScheme = PreviousScheme.OSSU

      val exclusion = OssExcludedTrader(
        vrn = Vrn(vrn),
        exclusionReason = Some(ExclusionReason.FailsToComply),
        effectiveDate = Some(LocalDate.of(2022, 2, 1)),
        quarantined = Some(true)
      )

      val ossDisplayRegistration = OssRegistration(
        vrn = Vrn(vrn),
        registeredCompanyName = "Company Name",
        tradingNames = Seq("Trade1", "Trade2"),
        vatDetails = mock[OssVatDetails],
        euRegistrations = Seq.empty,
        contactDetails = OssContactDetails("Test name", "0123456789", "test@test.com"),
        websites = Seq("https://example.com"),
        commencementDate = LocalDate.now(),
        previousRegistrations = Seq.empty,
        bankDetails = BankDetails("Test name", None, Iban("GB33BUKB20201555555555").value),
        isOnlineMarketplace = false,
        niPresence = None,
        dateOfFirstSale = Some(LocalDate.now()),
        submissionReceived = Some(Instant.now()),
        lastUpdated = Some(Instant.now()),
        excludedTrader = Some(exclusion),
        transferringMsidEffectiveFromDate = None,
        nonCompliantReturns = None,
        nonCompliantPayments = None,
        adminUse = mock[OssAdminUse]
      )

      val expectedResponse: CoreRegistrationValidationResult = coreValidationResponses.copy(
        searchId = vrn,
        searchIdIssuedBy = countryCode,
        matches = Seq.empty
      )

      when(ossRegistrationService.getLatestOssRegistration(Vrn(vrn))) thenReturn ossDisplayRegistration.toFuture
      when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(expectedResponse).toFuture

      val expectedMatch = Match(
        traderId = TraderId(vrn),
        intermediary = None,
        memberState = countryCode,
        exclusionStatusCode = Some(4),
        exclusionDecisionDate = None,
        exclusionEffectiveDate = Some("2022-02-01"),
        nonCompliantReturns = None,
        nonCompliantPayments = None
      )

      val service = new CoreRegistrationValidationService(
        mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
      )

      val result = service.searchScheme(vrn, previousScheme, None, countryCode).futureValue

      result `mustBe` Some(expectedMatch)
      verifyNoInteractions(mockAuditService)
    }

    "must return exception when server responds with an error" in {

      val previousScheme: PreviousScheme = PreviousScheme.OSSU

      val errorCode = Gen.oneOf(BAD_REQUEST, NOT_FOUND, INTERNAL_SERVER_ERROR).sample.value

      when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Left(UnexpectedResponseStatus(errorCode, "error")).toFuture

      val coreRegistrationValidationService = new CoreRegistrationValidationService(
        mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
      )

      val response = intercept[Exception](coreRegistrationValidationService.searchScheme(iossNumber, previousScheme, None, countryCode).futureValue)

      response.getMessage must include("Error while validating core registration")
      verifyNoInteractions(mockAuditService)
    }
  }

  ".searchTraderId" - {

    "must audit the event" - {

      val countryCode: String = "GB"
      val ukReferenceNumber: String = arbitrary[String].sample.value

      val coreRegistrationRequest = baseCoreRegistrationRequest(
        source = "TraderId",
        searchIdIssuedBy = countryCode,
        scheme = Some("IOSS"),
        searchId = ukReferenceNumber
      )

      "must return a match for a UTR or NINO" in {

        val aMatch: Match = genericMatch
          .copy(memberState = countryCode)

        val ukCoreValidationResponses: CoreRegistrationValidationResult = coreValidationResponses
          .copy(searchIdIssuedBy = countryCode)
          .copy(matches = Seq(aMatch))

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(ukCoreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val service = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, ukCoreValidationResponses
        )

        val result = service.searchTraderId(ukReferenceNumber).futureValue

        result `mustBe` Some(aMatch)
        verify(mockValidateCoreRegistrationConnector, times(1)).validateCoreRegistration(eqTo(coreRegistrationRequest))(any())
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }

      "must return None when no matches are found" in {

        val coreRegistrationRequest = baseCoreRegistrationRequest(
          source = "TraderId",
          searchIdIssuedBy = countryCode,
          scheme = Some("IOSS"),
          searchId = ukReferenceNumber
        )

        val ukCoreValidationResponses: CoreRegistrationValidationResult = coreValidationResponses
          .copy(searchIdIssuedBy = countryCode)
          .copy(matches = Seq.empty)

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(ukCoreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val service = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, ukCoreValidationResponses
        )

        val result = service.searchTraderId(ukReferenceNumber).futureValue

        result `mustBe` None
        verify(mockValidateCoreRegistrationConnector, times(1)).validateCoreRegistration(eqTo(coreRegistrationRequest))(any())
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }
    }
  }

  ".searchForeignTaxReference" - {

    "must audit the event" - {

      val country: Country = Gen.oneOf(Country.allCountries).sample.value
      val foreignTaxReference: String = arbitrary[String].sample.value

      val coreRegistrationRequest = baseCoreRegistrationRequest(
        source = "TraderId",
        searchIdIssuedBy = country.code,
        scheme = Some("IOSS"),
        searchId = foreignTaxReference
      )

      "must return a match for a foreign tax reference number" in {

        val foreignMatch: Match = genericMatch
          .copy(memberState = country.code)

        val foreignCoreValidationResponses: CoreRegistrationValidationResult = coreValidationResponses
          .copy(searchIdIssuedBy = country.code)
          .copy(matches = Seq(foreignMatch))

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(foreignCoreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val service = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, foreignCoreValidationResponses
        )

        val result = service.searchForeignTaxReference(foreignTaxReference, country.code).futureValue

        result `mustBe` Some(foreignMatch)
        verify(mockValidateCoreRegistrationConnector, times(1)).validateCoreRegistration(eqTo(coreRegistrationRequest))(any())
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }

      "must return None when no matches are found" in {

        val country: Country = Country("AL", "Albania")

        val coreRegistrationRequest = baseCoreRegistrationRequest(
          source = "TraderId",
          searchIdIssuedBy = country.code,
          scheme = Some("IOSS"),
          searchId = foreignTaxReference
        )

        val foreignCoreValidationResponses: CoreRegistrationValidationResult = coreValidationResponses
          .copy(searchIdIssuedBy = country.code)
          .copy(matches = Seq.empty)

        when(mockValidateCoreRegistrationConnector.validateCoreRegistration(any())(any())) thenReturn Right(foreignCoreValidationResponses).toFuture
        doNothing().when(mockAuditService).audit(any())(any(), any())

        val service = new CoreRegistrationValidationService(
          mockValidateCoreRegistrationConnector, iossRegistrationService, ossRegistrationService, mockAuditService
        )

        val expectedAuditEvent: CoreRegistrationAuditModel = CoreRegistrationAuditModel.build(
          coreRegistrationRequest, foreignCoreValidationResponses
        )

        val result = service.searchForeignTaxReference(foreignTaxReference, country.code).futureValue

        result `mustBe` None
        verify(mockValidateCoreRegistrationConnector, times(1)).validateCoreRegistration(eqTo(coreRegistrationRequest))(any())
        verify(mockAuditService, times(1)).audit(eqTo(expectedAuditEvent))(any(), any())
      }
    }
  }
}

