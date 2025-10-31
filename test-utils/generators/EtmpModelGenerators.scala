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

package generators

import models.PreviousScheme
import models.domain.{PreviousRegistration, PreviousSchemeDetails, PreviousSchemeNumbers}
import models.etmp.*
import models.etmp.amend.EtmpAmendRegistrationChangeLog
import models.etmp.display.{EtmpDisplayCustomerIdentification, EtmpDisplayEuRegistrationDetails, EtmpDisplayRegistration, EtmpDisplaySchemeDetails, RegistrationWrapper}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

import java.time.{LocalDate, LocalDateTime}

trait EtmpModelGenerators {
  self: ModelGenerators =>

  implicit lazy val arbitraryEtmpAdministration: Arbitrary[EtmpAdministration] =
    Arbitrary {
      for {
        messageType <- Gen.oneOf(EtmpMessageType.values)
      } yield EtmpAdministration(messageType, "IOSS")
    }

  implicit lazy val arbitraryCustomerIdentification: Arbitrary[EtmpCustomerIdentification] =
    Arbitrary {
      for {
        etmpIdType <- Gen.oneOf(EtmpIdType.values)
        vrn <- Gen.alphaStr
        intermediaryDetails <- arbitraryIntermediaryDetails.arbitrary
      } yield EtmpCustomerIdentification(etmpIdType, vrn, intermediaryDetails.intermediaryNumber)
    }

  implicit lazy val arbitraryEtmpCustomerIdentification: Arbitrary[EtmpDisplayCustomerIdentification] =
    Arbitrary {
      for {
        etmpIdType <- Gen.oneOf(EtmpIdType.values)
        vrn <- Gen.alphaStr
      } yield EtmpDisplayCustomerIdentification(etmpIdType, vrn)
    }
  
  implicit lazy val genIntermediaryNumber: Gen[String] = {
    for {
      intermediaryNumber <- Gen.listOfN(12, Gen.alphaChar).map(_.mkString)
    } yield intermediaryNumber
  }

  implicit lazy val arbitraryVatNumberTraderId: Arbitrary[VatNumberTraderId] =
    Arbitrary {
      for {
        vatNumber <- Gen.alphaNumStr
      } yield VatNumberTraderId(vatNumber)
    }

  implicit lazy val arbitraryTaxRefTraderID: Arbitrary[TaxRefTraderID] =
    Arbitrary {
      for {
        taxReferenceNumber <- Gen.alphaNumStr
      } yield TaxRefTraderID(taxReferenceNumber)
    }

  implicit lazy val arbitraryEtmpTradingName: Arbitrary[EtmpTradingName] =
    Arbitrary {
      for {
        tradingName <- Gen.alphaStr
      } yield EtmpTradingName(tradingName)
    }

  implicit lazy val arbitraryEtmpWebsite: Arbitrary[EtmpWebsite] =
    Arbitrary {
      for {
        websiteAddress <- Gen.alphaStr
      } yield EtmpWebsite(websiteAddress)
    }

  implicit lazy val arbitraryEtmpOtherIossIntermediaryRegistrations: Arbitrary[EtmpOtherIossIntermediaryRegistrations] =
    Arbitrary {
      for {
        countryCode <- Gen.listOfN(2, Gen.alphaChar).map(_.mkString)
        intermediaryNumber <- Gen.listOfN(12, Gen.alphaChar).map(_.mkString)
      } yield EtmpOtherIossIntermediaryRegistrations(countryCode, intermediaryNumber)
    }

  implicit lazy val arbitraryEtmpIntermediaryDetails: Arbitrary[EtmpIntermediaryDetails] =
    Arbitrary {
      for {
        amountOfOtherRegistrations <- Gen.chooseNum(1, 5)
        otherRegistrationDetails <- Gen.listOfN(amountOfOtherRegistrations, arbitraryEtmpOtherIossIntermediaryRegistrations.arbitrary)
      } yield EtmpIntermediaryDetails(otherRegistrationDetails)
    }

  implicit lazy val arbitraryEtmpOtherAddress: Arbitrary[EtmpOtherAddress] =
    Arbitrary {
      for {
        issuedBy <- Gen.listOfN(2, Gen.alphaChar).map(_.mkString)
        tradingName <- Gen.listOfN(20, Gen.alphaChar).map(_.mkString)
        addressLine1 <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
        addressLine2 <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
        townOrCity <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
        regionOrState <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
        postcode <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
      } yield EtmpOtherAddress(
        issuedBy,
        Some(tradingName),
        addressLine1,
        Some(addressLine2),
        townOrCity,
        Some(regionOrState),
        Some(postcode)
      )
    }

  implicit lazy val arbitraryPreviousIossSchemeDetails: Arbitrary[PreviousSchemeNumbers] =
    Arbitrary {
      PreviousSchemeNumbers("12345667")
    }


  implicit lazy val arbitraryEtmpClientDetails: Arbitrary[EtmpClientDetails] = {
    Arbitrary {
      for {
        clientName <- Gen.alphaStr
        clientIossID <- Gen.alphaNumStr
        clientExcluded <- arbitrary[Boolean]
      } yield {
        EtmpClientDetails(
          clientName = clientName,
          clientIossID = clientIossID,
          clientExcluded = clientExcluded
        )
      }
    }
  }
  
  implicit lazy val arbitraryEtmpBankDetails: Arbitrary[EtmpBankDetails] = {
    Arbitrary {
      for {
        accountName <- arbitrary[String]
        bic <- arbitraryBic.arbitrary
        iban <- arbitraryIban.arbitrary
      } yield {
        EtmpBankDetails(
          accountName = accountName,
          bic = Some(bic),
          iban = iban
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpDisplayEuRegistrationDetails: Arbitrary[EtmpDisplayEuRegistrationDetails] = {
    Arbitrary {
      for {
        issuedBy <- arbitraryCountry.arbitrary.map(_.code)
        vatNumber <- arbitraryEuVatNumber
        taxIdentificationNumber <- genEuTaxReference
        fixedEstablishmentTradingName <- arbitraryEtmpTradingName.arbitrary.map(_.tradingName)
        fixedEstablishmentAddressLine1 <- Gen.alphaStr
        fixedEstablishmentAddressLine2 <- Gen.alphaStr
        townOrCity <- Gen.alphaStr
        regionOrState <- Gen.alphaStr
        postcode <- Gen.alphaStr
      } yield {
        EtmpDisplayEuRegistrationDetails(
          issuedBy = issuedBy,
          vatNumber = Some(vatNumber),
          taxIdentificationNumber = Some(taxIdentificationNumber),
          fixedEstablishmentTradingName = fixedEstablishmentTradingName,
          fixedEstablishmentAddressLine1 = fixedEstablishmentAddressLine1,
          fixedEstablishmentAddressLine2 = Some(fixedEstablishmentAddressLine2),
          townOrCity = townOrCity,
          regionOrState = Some(regionOrState),
          postcode = Some(postcode)
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpPreviousEuRegistrationDetails: Arbitrary[EtmpPreviousEuRegistrationDetails] = {
    Arbitrary {
      for {
        issuedBy <- arbitraryCountry.arbitrary.map(_.code)
        registrationNumber <- arbitrary[String]
        schemeType <- Gen.oneOf(SchemeType.values)
        intermediaryNumber <- genIntermediaryNumber
      } yield {
        EtmpPreviousEuRegistrationDetails(
          issuedBy = issuedBy,
          registrationNumber = registrationNumber,
          schemeType = schemeType,
          intermediaryNumber = Some(intermediaryNumber)
        )
      }
    }
  }
  
  implicit lazy val arbitraryPreviousSchemeNumbers: Arbitrary[PreviousSchemeNumbers] = {
    Arbitrary {
      for {
        previousSchemeNumber <- arbitrary[String]
      } yield {
        PreviousSchemeNumbers(
          previousSchemeNumber = previousSchemeNumber
        )
      }
    }
  }
  
  implicit lazy val arbitraryPreviousSchemeDetails: Arbitrary[PreviousSchemeDetails] = {
    Arbitrary {
      for {
        previousScheme <- Gen.oneOf(PreviousScheme.values)
        previousSchemeNumbers <- arbitraryPreviousSchemeNumbers.arbitrary
      } yield {
        PreviousSchemeDetails(
          previousScheme = previousScheme,
          previousSchemeNumbers = previousSchemeNumbers,
          nonCompliantDetails = None
        )
      }
    }
  }
  
  implicit lazy val arbitraryPreviousRegistration: Arbitrary[PreviousRegistration] = {
    Arbitrary {
      for {
        previousEuCountry <- arbitraryCountry.arbitrary
        previousSchemesDetails <- Gen.listOfN(2, arbitraryPreviousSchemeDetails.arbitrary)
      } yield {
        PreviousRegistration(
          previousEuCountry = previousEuCountry,
          previousSchemesDetails = previousSchemesDetails
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpAmendRegistrationChangeLog: Arbitrary[EtmpAmendRegistrationChangeLog] = {
    Arbitrary {
      for {
        tradingNames <- arbitrary[Boolean]
        fixedEstablishments <- arbitrary[Boolean]
        contactDetails <- arbitrary[Boolean]
        bankDetails <- arbitrary[Boolean]
        reRegistration <- arbitrary[Boolean]
        otherAddress <- arbitrary[Boolean]
      } yield {
        EtmpAmendRegistrationChangeLog(
          tradingNames = tradingNames,
          fixedEstablishments = fixedEstablishments,
          contactDetails = contactDetails,
          bankDetails = bankDetails,
          reRegistration = reRegistration,
          otherAddress = otherAddress
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpDisplaySchemeDetails: Arbitrary[EtmpDisplaySchemeDetails] = {
    Arbitrary {
      for {
        commencementDate <- arbitrary[LocalDate].map(_.toString)
        euRegistrationDetails <- Gen.listOfN(3, arbitraryEtmpDisplayEuRegistrationDetails.arbitrary)
        contactName <- Gen.alphaStr
        businessTelephoneNumber <- Gen.alphaNumStr
        businessEmailId <- Gen.alphaStr
        unusableStatus <- arbitrary[Boolean]
        nonCompliant <- Gen.oneOf("1", "2")
        previousEURegistrationDetails <- Gen.listOfN(3, arbitraryEtmpPreviousEuRegistrationDetails.arbitrary)
        websites <- Gen.listOfN(3, arbitraryEtmpWebsite.arbitrary)
      } yield {
        EtmpDisplaySchemeDetails(
          commencementDate = commencementDate,
          euRegistrationDetails = euRegistrationDetails,
          contactName = contactName,
          businessTelephoneNumber = businessTelephoneNumber,
          businessEmailId = businessEmailId,
          unusableStatus = unusableStatus,
          nonCompliantReturns = Some(nonCompliant),
          nonCompliantPayments = Some(nonCompliant),
          previousEURegistrationDetails = previousEURegistrationDetails,
          websites = websites
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpExclusion: Arbitrary[EtmpExclusion] = {
    Arbitrary {
      for {
        exclusionReason <- Gen.oneOf(EtmpExclusionReason.values)
        effectiveDate <- arbitrary[LocalDate]
        decisionDate <- arbitrary[LocalDate]
        quarantine <- arbitrary[Boolean]
      } yield {
        EtmpExclusion(
          exclusionReason = exclusionReason,
          effectiveDate = effectiveDate,
          decisionDate = decisionDate,
          quarantine = quarantine
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpEuRegistrationDetails: Arbitrary[EtmpEuRegistrationDetails] = {
    Arbitrary {
      for {
        countryOfRegistration <- arbitraryCountry.arbitrary.map(_.code)
        traderId <- Gen.oneOf(arbitraryVatNumberTraderId.arbitrary, arbitraryTaxRefTraderID.arbitrary)
        tradingName <- Gen.alphaStr
        fixedEstablishmentAddressLine1 <- Gen.alphaStr
        fixedEstablishmentAddressLine2 <- Gen.option(Gen.alphaStr)
        townOrCity <- Gen.alphaStr
        regionOrState <- Gen.option(Gen.alphaStr)
        postcode <- Gen.option(Gen.alphaStr)
      } yield {
        EtmpEuRegistrationDetails(
          countryOfRegistration = countryOfRegistration,
          traderId = traderId,
          tradingName = tradingName,
          fixedEstablishmentAddressLine1 = fixedEstablishmentAddressLine1,
          fixedEstablishmentAddressLine2 = fixedEstablishmentAddressLine2,
          townOrCity = townOrCity,
          regionOrState = regionOrState,
          postcode = postcode
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpSchemeDetails: Arbitrary[EtmpSchemeDetails] = {
    Arbitrary {
      for {
        commencementDate <- arbitrary[LocalDate].map(_.toString)
        euRegistrationDetails <- Gen.listOfN(2, arbitraryEtmpEuRegistrationDetails.arbitrary)
        previousEURegistrationDetails <- Gen.listOfN(2, arbitraryEtmpPreviousEuRegistrationDetails.arbitrary)
        websites <- Gen.option(Gen.listOfN(2, arbitraryEtmpWebsite.arbitrary))
        contactName <- Gen.alphaStr
        businessTelephoneNumber <- Gen.numStr
        businessEmailId <- Gen.alphaStr
        nonCompliantReturns <- Gen.option(Gen.oneOf("1", "2"))
        nonCompliantPayments <- Gen.option(Gen.oneOf("1", "2"))
      } yield {
        EtmpSchemeDetails(
          commencementDate = commencementDate,
          euRegistrationDetails = euRegistrationDetails,
          previousEURegistrationDetails = previousEURegistrationDetails,
          websites = websites,
          contactName = contactName,
          businessTelephoneNumber = businessTelephoneNumber,
          businessEmailId = businessEmailId,
          nonCompliantReturns = nonCompliantReturns,
          nonCompliantPayments = nonCompliantPayments
        )
      }
    }
  }


  implicit lazy val arbitraryEtmpAdminUse: Arbitrary[EtmpAdminUse] = {
    Arbitrary {
      for {
        changeDate <- Gen.option(arbitrary[LocalDateTime])
      } yield EtmpAdminUse(changeDate = changeDate)
    }
  }
  
  implicit lazy val arbitraryEtmpDisplayRegistration: Arbitrary[EtmpDisplayRegistration] = {
    Arbitrary {
      for {
        customerIdentification <- arbitraryEtmpCustomerIdentification.arbitrary
        tradingNames <- Gen.listOfN(3, arbitraryEtmpTradingName.arbitrary)
        clientDetails <- Gen.listOfN(3, arbitraryEtmpClientDetails.arbitrary)
        otherAddress <- arbitraryEtmpOtherAddress.arbitrary
        schemeDetails <- arbitraryEtmpDisplaySchemeDetails.arbitrary
        exclusions <- Gen.listOfN(1, arbitraryEtmpExclusion.arbitrary)
        bankDetails <- arbitraryEtmpBankDetails.arbitrary
        adminUse <- arbitraryEtmpAdminUse.arbitrary
      } yield {
        EtmpDisplayRegistration(
          customerIdentification = customerIdentification,
          tradingNames = tradingNames,
          clientDetails = clientDetails,
          otherAddress = Some(otherAddress),
          schemeDetails = schemeDetails,
          exclusions = exclusions,
          adminUse = adminUse
        )
      }
    }
  }
  
  implicit lazy val arbitraryRegistrationWrapper: Arbitrary[RegistrationWrapper] = {
    Arbitrary {
      for {
        vatInfo <- arbitraryVatCustomerInfo.arbitrary
        etmpDisplayRegistration <- arbitraryEtmpDisplayRegistration.arbitrary
      } yield {
        RegistrationWrapper(
          vatInfo = Some(vatInfo),
          etmpDisplayRegistration = etmpDisplayRegistration
        )
      }
    }
  }
}
