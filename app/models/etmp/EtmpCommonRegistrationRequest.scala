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

import formats.Format.eisDateFormatter
import models.{BusinessContactDetails, Country, UserAnswers}
import pages.*
import pages.tradingNames.HasTradingNamePage
import queries.AllWebsites
import queries.tradingNames.AllTradingNamesQuery
import services.etmp.{EtmpEuRegistrations, EtmpPreviousRegistrationsRequest}

import java.time.LocalDate

trait EtmpCommonRegistrationRequest extends EtmpEuRegistrations with EtmpPreviousRegistrationsRequest {

  def getOtherAddress(idType: EtmpIdType, answers: UserAnswers): Option[EtmpOtherAddress] = {
    idType match {
      case EtmpIdType.VRN =>
        None
      case _ =>
        Some(
          (for {
            basedInUK <- answers.get(BusinessBasedInUKPage)
            clientCountryBased <- idType match {
              case EtmpIdType.UTR | EtmpIdType.NINO =>
                Some(Country.unitedKingdomCountry)
              case _ =>
                answers.get(ClientCountryBasedPage)
            }
            businessAddress <- answers.get(ClientBusinessAddressPage)
            businessName <- answers.get(ClientBusinessNamePage)
          } yield {

            EtmpOtherAddress(
              issuedBy = clientCountryBased.code,
              tradingName = Some(businessName.name),
              addressLine1 = businessAddress.line1,
              addressLine2 = businessAddress.line2,
              townOrCity = businessAddress.townOrCity,
              regionOrState = businessAddress.stateOrRegion,
              postcode = businessAddress.postCode
            )
          }).getOrElse {
            val exception = new IllegalStateException("Didn't have a VRN, so must have a business address, trading name and country")
            logger.error(exception.getMessage, exception)
            throw exception
          }
        )
    }
  }

  def getSchemeDetails(answers: UserAnswers, commencementDate: LocalDate): EtmpSchemeDetails = {

    val businessContactDetails = getBusinessContactDetails(answers)

    val nonCompliantDetails = getMaximumNonCompliantDetails(answers)

    EtmpSchemeDetails(
      commencementDate = commencementDate.format(eisDateFormatter),
      euRegistrationDetails = getEuTaxRegistrations(answers),
      previousEURegistrationDetails = getPreviousRegistrationDetails(answers),
      websites = Some(getWebsites(answers)),
      contactName = businessContactDetails.fullName,
      businessTelephoneNumber = businessContactDetails.telephoneNumber,
      businessEmailId = businessContactDetails.emailAddress,
      nonCompliantReturns = nonCompliantDetails.flatMap(_.nonCompliantReturns.map(_.toString)),
      nonCompliantPayments = nonCompliantDetails.flatMap(_.nonCompliantPayments.map(_.toString))
    )
  }

  def getTradingNames(answers: UserAnswers): List[EtmpTradingName] = {
    answers.get(HasTradingNamePage) match {
      case Some(true) =>
        answers.get(AllTradingNamesQuery) match {
          case Some(tradingNames) =>
            for {
              tradingName <- tradingNames
            } yield EtmpTradingName(tradingName = tradingName.name)
          case Some(Nil) | None =>
            val exception = new IllegalStateException("Must have at least one trading name")
            logger.error(exception.getMessage, exception)
            throw exception
        }

      case Some(false) =>
        List.empty

      case None =>
        val exception = new IllegalStateException("Must select Yes if trading name is different")
        logger.error(exception.getMessage, exception)
        throw exception
    }
  }

  private def getBusinessContactDetails(answers: UserAnswers): BusinessContactDetails = {
    answers.get(BusinessContactDetailsPage) match {
      case Some(contactDetails) => contactDetails
      case _ =>
        val exception = new IllegalStateException("User must provide contact details")
        logger.error(exception.getMessage, exception)
        throw exception
    }
  }

  private def getWebsites(answers: UserAnswers): List[EtmpWebsite] =
    answers.get(AllWebsites) match {
      case Some(websites) =>
        for {
          website <- websites
        } yield EtmpWebsite(websiteAddress = website.site)
      case _ =>
        val exception = new IllegalStateException("User must have at least one website")
        logger.error(exception.getMessage, exception)
        throw exception
    }
}
