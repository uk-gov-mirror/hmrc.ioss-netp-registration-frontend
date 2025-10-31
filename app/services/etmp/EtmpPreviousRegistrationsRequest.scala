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

package services.etmp

import logging.Logging
import models.PreviousScheme.toEmtpSchemeType
import models.domain.PreviousSchemeDetails
import models.etmp.EtmpPreviousEuRegistrationDetails
import models.previousRegistrations.NonCompliantDetails
import models.{Country, CountryWithValidationDetails, UserAnswers}
import pages.previousRegistrations.PreviouslyRegisteredPage
import queries.previousRegistrations.AllPreviousRegistrationsQuery

trait EtmpPreviousRegistrationsRequest extends Logging {

  def getPreviousRegistrationDetails(answers: UserAnswers): Seq[EtmpPreviousEuRegistrationDetails] = {
    answers.get(PreviouslyRegisteredPage) match {
      case Some(true) =>
        answers.get(AllPreviousRegistrationsQuery) match {
          case Some(previousRegistrations) =>
            previousRegistrations.flatMap { previousRegistration =>
              previousRegistration.previousSchemesDetails.map { previousSchemeDetails =>
                processPreviousRegistrationDetails(previousRegistration.previousEuCountry, previousSchemeDetails)
              }
            }
          case None =>
            val exception = new IllegalStateException("User must provide previous Eu details when previously tax registered in the EU")
            logger.error(exception.getMessage, exception)
            throw exception
        }
      case Some(false) =>
        List.empty
      case _ =>
        val exception = new IllegalStateException("User must answer if they are previously tax registered in the EU")
        logger.error(exception.getMessage, exception)
        throw exception
    }
  }

  private def processPreviousRegistrationDetails(
                                                  previousEuCountry: Country,
                                                  previousSchemeDetails: PreviousSchemeDetails
                                                ): EtmpPreviousEuRegistrationDetails = {
    EtmpPreviousEuRegistrationDetails(
      issuedBy = previousEuCountry.code,
      registrationNumber = CountryWithValidationDetails.convertTaxIdentifierForTransfer(
        previousSchemeDetails.previousSchemeNumbers.previousSchemeNumber,
        previousEuCountry.code
      ),
      schemeType = toEmtpSchemeType(previousSchemeDetails.previousScheme),
      intermediaryNumber = None
    )
  }

  def getMaximumNonCompliantDetails(answers: UserAnswers): Option[NonCompliantDetails] = {
    answers.get(PreviouslyRegisteredPage) match {
      case Some(true) =>
        answers.get(AllPreviousRegistrationsQuery) match {
          case Some(previousRegistrations) =>
            val allNonCompliantDetails = previousRegistrations.flatMap(_.previousSchemesDetails).flatMap(_.nonCompliantDetails)
            if (allNonCompliantDetails.nonEmpty) {
              val maximumNonCompliantReturns = allNonCompliantDetails.maxBy(_.nonCompliantReturns).nonCompliantReturns
              val maximumNonCompliantPayments = allNonCompliantDetails.maxBy(_.nonCompliantPayments).nonCompliantPayments
              Some(NonCompliantDetails(maximumNonCompliantReturns, maximumNonCompliantPayments))
            } else {
              None
            }
          case _ => None
        }
      case _ =>
        None
    }
  }
}

