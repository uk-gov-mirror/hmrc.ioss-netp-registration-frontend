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

package models.etmp

import base.SpecBase
import config.Constants.{maxSchemes, maxTradingNames, maxWebsites}
import formats.Format.eisDateFormatter
import models.PreviousScheme.toEmtpSchemeType
import models.domain.{PreviousRegistration, PreviousSchemeDetails}
import models.previousRegistrations.NonCompliantDetails
import models.vatEuDetails.*
import models.{BusinessContactDetails, CountryWithValidationDetails, PreviousScheme, TradingName, UserAnswers, Website}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import pages.previousRegistrations.PreviouslyRegisteredPage
import pages.tradingNames.HasTradingNamePage
import pages.vatEuDetails.HasFixedEstablishmentPage
import pages.{BusinessContactDetailsPage, ClientHasVatNumberPage, ClientVatNumberPage}
import play.api.libs.json.{JsError, JsSuccess, Json}
import queries.euDetails.AllEuDetailsQuery
import queries.previousRegistrations.AllPreviousRegistrationsQuery
import queries.tradingNames.AllTradingNamesQuery
import queries.{AllWebsites, IntermediaryDetailsQuery}
import testutils.RegistrationData.etmpRegistrationRequest

import java.time.LocalDate

class EtmpRegistrationRequestSpec extends SpecBase {

  private val numberOfRegistrations: Int = 8

  private def convertToTraderId(euDetails: EuDetails): Option[TraderId] = {
    euDetails.registrationType match {
      case Some(RegistrationType.VatNumber) =>
        val convertedVatNumber = CountryWithValidationDetails.convertTaxIdentifierForTransfer(euDetails.euVatNumber.value, euDetails.euCountry.code)
        Some(VatNumberTraderId(convertedVatNumber))
      case Some(RegistrationType.TaxId) =>
        Some(TaxRefTraderID(euDetails.euTaxReference.value))
      case _ => None
    }
  }

  "EtmpRegistrationRequest" - {

    "must deserialise/serialise to and from EtmpRegistrationRequest" in {

      val json = Json.obj(
        "administration" -> etmpRegistrationRequest.administration,
        "customerIdentification" -> etmpRegistrationRequest.customerIdentification,
        "tradingNames" -> etmpRegistrationRequest.tradingNames,
        "intermediaryDetails" -> etmpRegistrationRequest.intermediaryDetails,
        "otherAddress" -> etmpRegistrationRequest.otherAddress,
        "schemeDetails" -> etmpRegistrationRequest.schemeDetails
      )

      val expectedResult = EtmpRegistrationRequest(
        administration = etmpRegistrationRequest.administration,
        customerIdentification = etmpRegistrationRequest.customerIdentification,
        tradingNames = etmpRegistrationRequest.tradingNames,
        intermediaryDetails = etmpRegistrationRequest.intermediaryDetails,
        otherAddress = etmpRegistrationRequest.otherAddress,
        schemeDetails = etmpRegistrationRequest.schemeDetails,
        bankDetails = etmpRegistrationRequest.bankDetails
      )

      Json.toJson(expectedResult) mustBe json
      json.validate[EtmpRegistrationRequest] mustBe JsSuccess(expectedResult)
    }

    ".buildEtmpRegistrationRequest" - {

      val tradingNames: List[TradingName] = Gen.listOfN(maxTradingNames, arbitrary[TradingName]).sample.value
      val previousRegistration: PreviousRegistration = PreviousRegistration(
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
      val previousEuRegistrations: List[PreviousRegistration] = Gen.listOfN(numberOfRegistrations, previousRegistration).sample.value
      val euRegistration: EuDetails = EuDetails(
        euCountry = arbitraryCountry.arbitrary.sample.value,
        hasFixedEstablishment = Some(true),
        registrationType = Some(arbitraryRegistrationType.arbitrary.sample.value),
        euVatNumber = Some(arbitraryEuVatNumber.sample.value),
        euTaxReference = Some(arbitraryEuTaxReference.sample.value),
        tradingNameAndBusinessAddress = Some(arbitraryTradingNameAndBusinessAddress.arbitrary.sample.value)
      )
      val euRegistrations = Gen.listOfN(numberOfRegistrations, euRegistration).sample.value
      val websites = Gen.listOfN(maxWebsites, arbitrary[Website]).sample.value
      val genBusinessContactDetails = BusinessContactDetails(
        fullName = arbitrary[String].sample.value,
        telephoneNumber = arbitrary[String].sample.value,
        emailAddress = arbitrary[String].sample.value
      )

      val userAnswers: UserAnswers = emptyUserAnswersWithVatInfo
        .set(ClientHasVatNumberPage, true).success.value
        .set(ClientVatNumberPage, "123456789").success.value
        .set(HasTradingNamePage, true).success.value
        .set(AllTradingNamesQuery, tradingNames).success.value
        .set(PreviouslyRegisteredPage, true).success.value
        .set(AllPreviousRegistrationsQuery, previousEuRegistrations).success.value
        .set(HasFixedEstablishmentPage, true).success.value
        .set(AllEuDetailsQuery, euRegistrations).success.value
        .set(AllWebsites, websites).success.value
        .set(BusinessContactDetailsPage, genBusinessContactDetails).success.value
        .set(IntermediaryDetailsQuery, intermediaryDetails).success.value
        .copy(vatInfo = Some(vatCustomerInfo))

      "must convert userAnswers to an EtmpRegistrationRequest" in {

        val convertToEtmpTradingNames: List[EtmpTradingName] =
          for {
            tradingName <- tradingNames
          } yield EtmpTradingName(tradingName.name)

        val convertToEtmpWebsite: List[EtmpWebsite] =
          for {
            website <- websites
          } yield EtmpWebsite(website.site)

        val etmpPreviousEuRegistrationDetails: List[EtmpPreviousEuRegistrationDetails] = {
          for {
            previousEuRegistration <- previousEuRegistrations
            previousScheme <- previousEuRegistration.previousSchemesDetails
          } yield {
            EtmpPreviousEuRegistrationDetails(
              issuedBy = previousEuRegistration.previousEuCountry.code,
              registrationNumber = previousScheme.previousSchemeNumbers.previousSchemeNumber,
              schemeType = toEmtpSchemeType(previousScheme.previousScheme),
              intermediaryNumber = None
            )
          }
        }

        val etmpEuRegistrationDetails: List[EtmpEuRegistrationDetails] = {
          for {
            euDetails <- euRegistrations
            traderId <- convertToTraderId(euDetails)
          } yield {
            EtmpEuRegistrationDetails(
              countryOfRegistration = euDetails.euCountry.code,
              traderId = traderId,
              tradingName = euDetails.tradingNameAndBusinessAddress.map(_.tradingName.name).value,
              fixedEstablishmentAddressLine1 = euDetails.tradingNameAndBusinessAddress.map(_.address.line1).value,
              fixedEstablishmentAddressLine2 = euDetails.tradingNameAndBusinessAddress.map(_.address.line2).value,
              townOrCity = euDetails.tradingNameAndBusinessAddress.map(_.address.townOrCity).value,
              regionOrState = euDetails.tradingNameAndBusinessAddress.map(_.address.stateOrRegion).value,
              postcode = euDetails.tradingNameAndBusinessAddress.map(_.address.postCode).value
            )
          }
        }

        val (maxByReturns, maxByPayments) = {
          val x = previousEuRegistrations.flatMap(_.previousSchemesDetails.flatMap(_.nonCompliantDetails))
          val allReturns = x.flatMap(_.nonCompliantReturns)
          val allPayments = x.flatMap(_.nonCompliantPayments)
          (allReturns.maxOption.map(_.toString), allPayments.maxOption.map(_.toString))
        }

        val etmpSchemeDetails = EtmpSchemeDetails(
          commencementDate = LocalDate.now(stubClockAtArbitraryDate).format(eisDateFormatter),
          euRegistrationDetails = etmpEuRegistrationDetails,
          previousEURegistrationDetails = etmpPreviousEuRegistrationDetails,
          websites = Some(convertToEtmpWebsite),
          contactName = genBusinessContactDetails.fullName,
          businessTelephoneNumber = genBusinessContactDetails.telephoneNumber,
          businessEmailId = genBusinessContactDetails.emailAddress,
          nonCompliantReturns = maxByReturns,
          nonCompliantPayments = maxByPayments,
        )

        val etmpRegistrationRequest: EtmpRegistrationRequest = EtmpRegistrationRequest(
          administration = EtmpAdministration(messageType = EtmpMessageType.IOSSIntAddClient),
          customerIdentification = EtmpCustomerIdentification(EtmpIdType.VRN, vrn.vrn, intermediaryDetails.intermediaryNumber),
          tradingNames = convertToEtmpTradingNames,
          intermediaryDetails = None,
          otherAddress = None,
          schemeDetails = etmpSchemeDetails,
          bankDetails = None
        )

        EtmpRegistrationRequest.buildEtmpRegistrationRequest(
          userAnswers,
          LocalDate.now(stubClockAtArbitraryDate)
        ) mustBe etmpRegistrationRequest
      }
    }

    "must handle missing fields during deserialization" in {

      val json = Json.obj()

      json.validate[EtmpRegistrationRequest] mustBe a[JsError]
    }

    "must handle invalid data during deserialization" in {

      val json = Json.obj(
        "administration" -> 12345,
        "customerIdentification" -> etmpRegistrationRequest.customerIdentification,
        "tradingNames" -> etmpRegistrationRequest.tradingNames,
        "schemeDetails" -> etmpRegistrationRequest.schemeDetails,
        "bankDetails" -> etmpRegistrationRequest.bankDetails
      )
      json.validate[EtmpRegistrationRequest] mustBe a[JsError]
    }
  }
}

