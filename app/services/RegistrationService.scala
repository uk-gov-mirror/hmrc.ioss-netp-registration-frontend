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

import connectors.RegistrationConnector
import connectors.RegistrationHttpParser.{AmendRegistrationResultResponse, RegistrationResultResponse}
import logging.Logging
import models.domain.PreviousSchemeDetails
import models.etmp.EtmpRegistrationRequest.buildEtmpRegistrationRequest
import models.etmp.amend.EtmpAmendRegistrationRequest
import models.etmp.amend.EtmpAmendRegistrationRequest.*
import models.etmp.display.*
import models.etmp.{EtmpIdType, EtmpOtherAddress, EtmpPreviousEuRegistrationDetails, EtmpTradingName}
import models.previousRegistrations.PreviousRegistrationDetails
import models.vatEuDetails.{EuDetails, RegistrationType, TradingNameAndBusinessAddress}
import models.{BusinessContactDetails, ClientBusinessName, Country, InternationalAddress, TradingName, UserAnswers, Website}
import pages.previousRegistrations.PreviouslyRegisteredPage
import pages.tradingNames.HasTradingNamePage
import pages.vatEuDetails.HasFixedEstablishmentPage
import pages.{BusinessBasedInUKPage, BusinessContactDetailsPage, ClientBusinessAddressPage, ClientBusinessNamePage, ClientCountryBasedPage, ClientHasUtrNumberPage, ClientHasVatNumberPage, ClientTaxReferencePage, ClientUtrNumberPage, ClientVatNumberPage, ClientsNinoNumberPage}
import queries.AllWebsites
import queries.euDetails.AllEuDetailsQuery
import queries.previousRegistrations.AllPreviousRegistrationsQuery
import queries.tradingNames.AllTradingNamesQuery
import services.etmp.EtmpEuRegistrations
import uk.gov.hmrc.http.HeaderCarrier
import utils.CheckUkBased.isUkBasedNetp

import java.time.{Clock, LocalDate}
import javax.inject.Inject
import scala.concurrent.Future
import scala.util.Try

class RegistrationService @Inject()(
                                     clock: Clock,
                                     registrationConnector: RegistrationConnector
                                   ) extends EtmpEuRegistrations with Logging {

  def createRegistration(answers: UserAnswers)(implicit hc: HeaderCarrier): Future[RegistrationResultResponse] = {
    val commencementDate = LocalDate.now(clock)
    registrationConnector.createRegistration(buildEtmpRegistrationRequest(answers, commencementDate))
  }

  def amendRegistration(answers: UserAnswers,
                        registration: EtmpDisplayRegistration,
                        iossNumber: String,
                        rejoin: Boolean = false)(implicit hc: HeaderCarrier): Future[AmendRegistrationResultResponse] = {

    val commencementDate = LocalDate.parse(registration.schemeDetails.commencementDate)

    registrationConnector.amendRegistration(
      buildEtmpAmendRegistrationRequest(
        answers = answers,
        registration = registration,
        commencementDate = commencementDate,
        iossNumber = iossNumber,
        rejoin = rejoin
      )
    )
  }

  def toUserAnswers(userId: String, registrationWrapper: RegistrationWrapper): Future[UserAnswers] = {

    val etmpTradingNames: Seq[EtmpTradingName] = registrationWrapper.etmpDisplayRegistration.tradingNames
    val maybeOtherAddress: Option[EtmpOtherAddress] = registrationWrapper.etmpDisplayRegistration.otherAddress
    val schemeDetails: EtmpDisplaySchemeDetails = registrationWrapper.etmpDisplayRegistration.schemeDetails
    val maybePreviousEuRegistrationDetails: Seq[EtmpPreviousEuRegistrationDetails] =
      registrationWrapper.etmpDisplayRegistration.schemeDetails.previousEURegistrationDetails

    val hasUkBasedAddress: Boolean = isUkBasedNetp(registrationWrapper.vatInfo, registrationWrapper.etmpDisplayRegistration.otherAddress)

    val userAnswers = for {
      businessBasedInUk <- UserAnswers(
        id = userId,
        vatInfo = registrationWrapper.vatInfo
      ).set(BusinessBasedInUKPage, hasUkBasedAddress)

      taxIdUA <- getTaxIdentifierAndNum(businessBasedInUk, registrationWrapper.etmpDisplayRegistration.customerIdentification)

      hasTradingNamesUA <- taxIdUA.set(HasTradingNamePage, etmpTradingNames.nonEmpty)
      tradingNamesUA <- if (etmpTradingNames.nonEmpty) {
        hasTradingNamesUA.set(AllTradingNamesQuery, convertTradingNames(etmpTradingNames).toList)
      } else {
        Try(hasTradingNamesUA)
      }

      hasPreviousEuRegistrationsUA <- tradingNamesUA.set(PreviouslyRegisteredPage, maybePreviousEuRegistrationDetails.exists(_.registrationNumber.nonEmpty))
      previousEuRegistrationsUA <- if (maybePreviousEuRegistrationDetails.exists(_.registrationNumber.nonEmpty)) {
        hasPreviousEuRegistrationsUA.set(AllPreviousRegistrationsQuery, convertEtmpPreviousEuRegistrations(maybePreviousEuRegistrationDetails))
      } else {
        Try(hasPreviousEuRegistrationsUA)
      }

      hasFixedEstablishment <- previousEuRegistrationsUA.set(HasFixedEstablishmentPage, schemeDetails.euRegistrationDetails.nonEmpty)
      euFixedEstablishmentUA <- if (schemeDetails.euRegistrationDetails.nonEmpty) {
        hasFixedEstablishment.set(AllEuDetailsQuery, convertEuFixedEstablishmentDetails(schemeDetails.euRegistrationDetails).toList)
      } else {
        Try(hasFixedEstablishment)
      }
      contactDetailsUA <- euFixedEstablishmentUA.set(BusinessContactDetailsPage, getContactDetails(schemeDetails))
      websiteUA <- implementWebsiteUserAnswers(contactDetailsUA, registrationWrapper.etmpDisplayRegistration)
      addressDetailsUA <- setNonVatAddressDetails(websiteUA, maybeOtherAddress)

      setClientCountryUA <- setClientCountry(addressDetailsUA, maybeOtherAddress)
      
    } yield setClientCountryUA

    Future.fromTry(userAnswers)
  }

  private[services] def setClientCountry(userAnswers: UserAnswers, optionEtmpOtherAddress: Option[EtmpOtherAddress]): Try[UserAnswers] = {

    (userAnswers.vatInfo, optionEtmpOtherAddress) match {

      case (_, Some(otherAddress)) => {
        val internationalAddress = convertNonUkAddress(Some(otherAddress))
        val country = internationalAddress.country.getOrElse {
          logger.error(s"Unable to retrieve a Country from the International Address provided in the Etmp Other Address from ETMP for amend journey.")
          throw new IllegalStateException(s"Unable to retrieve a Country from the International Address provided in the Etmp Other Address from ETMP for amend journey.")
        }
        for {
          nonVatCountryAnswers <- userAnswers.set(ClientCountryBasedPage, country)
        } yield {
          nonVatCountryAnswers
        }
      }

      case (Some(vatInfo), _) => {
        val country = Country.fromCountryCodeAllCountries(vatInfo.desAddress.countryCode).getOrElse {
          logger.error(s"Unable to retrieve a Country from the Country Code [${vatInfo.desAddress.countryCode}] provided in the Vat information returned from ETMP for amend journey.")
          throw new IllegalStateException(s"Unable to retrieve a Country from the Country Code [${vatInfo.desAddress.countryCode}] provided in the Vat information returned from ETMP for amend journey.")
        }
        for {
          nonVatCountryAnswers <- userAnswers.set(ClientCountryBasedPage, country)
        } yield {
          nonVatCountryAnswers
        }
      }

      case (_, _) =>
        Try(userAnswers)
    }
  }

  private[services] def setNonVatAddressDetails(userAnswers: UserAnswers, maybeOtherAddress: Option[EtmpOtherAddress]): Try[UserAnswers] = {
    
    if (maybeOtherAddress.isDefined) {
      val nonVatTradingName: String = maybeOtherAddress.flatMap(_.tradingName).getOrElse {
        logger.error(s"Unable to retrieve a Trading name from Other Address, required for client business naming without vat for amend journey.")
        throw new IllegalStateException(s"Unable to retrieve a Trading name from Other Address, required for client business naming without vat for for amend journey.")
      }
      for {
        nonVatBusinessAddress <- userAnswers.set(ClientBusinessAddressPage, convertNonUkAddress(maybeOtherAddress))
        nonVatTradingNameAnswers <- nonVatBusinessAddress.set(ClientBusinessNamePage, ClientBusinessName(nonVatTradingName))
      } yield nonVatTradingNameAnswers
    } else {
      Try(userAnswers)
    }
  }

  private[services] def getTaxIdentifierAndNum(userAnswers: UserAnswers, customerInfo: EtmpDisplayCustomerIdentification): Try[UserAnswers] = {
    customerInfo.idType match {
      case EtmpIdType.VRN => for {
        hasUkVatUA <- userAnswers.set(ClientHasVatNumberPage, true)
        userAnswersWithUkVatNum <- hasUkVatUA.set(ClientVatNumberPage, customerInfo.idValue)
      } yield userAnswersWithUkVatNum

      case EtmpIdType.UTR =>
        for {
          hasUtrUa <- userAnswers.set(ClientHasUtrNumberPage, true)
          userAnswersWithUtrNum <- hasUtrUa.set(ClientUtrNumberPage, customerInfo.idValue)
        } yield userAnswersWithUtrNum

      case EtmpIdType.FTR => for {
        userAnswersWithFtrNum <- userAnswers.set(ClientTaxReferencePage, customerInfo.idValue)
      } yield userAnswersWithFtrNum


      case EtmpIdType.NINO => for {
        hasUtrUa <- userAnswers.set(ClientHasUtrNumberPage, false)
        userAnswersWithNinoNum <- hasUtrUa.set(ClientsNinoNumberPage, customerInfo.idValue)
      } yield userAnswersWithNinoNum
    }
  }

  private def implementWebsiteUserAnswers(userAnswers: UserAnswers, customerInfo: EtmpDisplayRegistration): Try[UserAnswers] = {

    val websiteList: List[Website] = customerInfo.schemeDetails.websites.map { etmpWebsite =>
      Website(etmpWebsite.websiteAddress)
    }.toList

    for {
      websiteUserUA <- userAnswers.set(AllWebsites, websiteList)
    } yield websiteUserUA
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
      val exception = new IllegalStateException(s"Etmp Other Address must be present if client does not have a Vat number.")
      logger.error(exception.getMessage, exception)
      throw exception
    }
  }

  private def convertTradingNames(etmpTradingNames: Seq[EtmpTradingName]): Seq[TradingName] = {
    for {
      tradingName <- etmpTradingNames.map(_.tradingName)
    } yield TradingName(name = tradingName)
  }


  private def convertEtmpPreviousEuRegistrations(allEtmpPreviousEuRegistrationDetails: Seq[EtmpPreviousEuRegistrationDetails]): List[PreviousRegistrationDetails] = {
    val countrySchemaDetailsMapping: Map[Country, Seq[(Country, PreviousSchemeDetails)]] =
      allEtmpPreviousEuRegistrationDetails.map { etmpPreviousEuRegistrationDetails =>
        val country = Country.fromCountryCodeUnsafe(etmpPreviousEuRegistrationDetails.issuedBy)
        val details: PreviousSchemeDetails = PreviousSchemeDetails.fromEtmpPreviousEuRegistrationDetails(etmpPreviousEuRegistrationDetails)

        country -> details

      }.groupBy(_._1)

    countrySchemaDetailsMapping.map { case (country, countryPreviousSchemaDetails) =>
      PreviousRegistrationDetails(previousEuCountry = country, previousSchemesDetails = countryPreviousSchemaDetails.map(_._2))
    }.toList
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

  private def getCountry(countryCode: String): Country = {
    Country.fromCountryCodeAllCountries(countryCode) match {
      case Some(country) => country
      case _ =>
        val exception = new IllegalStateException(s"Unable to find country $countryCode")
        logger.error(exception.getMessage, exception)
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

  private def getContactDetails(schemeDetails: EtmpDisplaySchemeDetails): BusinessContactDetails = {
    BusinessContactDetails(
      fullName = schemeDetails.contactName,
      telephoneNumber = schemeDetails.businessTelephoneNumber,
      emailAddress = schemeDetails.businessEmailId
    )
  }
}
