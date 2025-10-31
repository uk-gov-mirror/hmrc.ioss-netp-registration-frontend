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

import logging.Logging
import models.UserAnswers
import pages.*
import play.api.libs.json.{Json, OFormat}
import queries.IntermediaryDetailsQuery

import java.time.LocalDate

final case class EtmpRegistrationRequest(
                                          administration: EtmpAdministration,
                                          customerIdentification: EtmpCustomerIdentification,
                                          tradingNames: Seq[EtmpTradingName],
                                          intermediaryDetails: Option[EtmpIntermediaryDetails],
                                          otherAddress: Option[EtmpOtherAddress],
                                          schemeDetails: EtmpSchemeDetails,
                                          bankDetails: Option[EtmpBankDetails]
                                        )

object EtmpRegistrationRequest extends EtmpCommonRegistrationRequest with Logging {

  implicit val format: OFormat[EtmpRegistrationRequest] = Json.format[EtmpRegistrationRequest]

  def buildEtmpRegistrationRequest(answers: UserAnswers, commencementDate: LocalDate): EtmpRegistrationRequest = {
    val customerIdentification = getCustomerIdentification(answers)
    EtmpRegistrationRequest(
      administration = EtmpAdministration(messageType = EtmpMessageType.IOSSIntAddClient),
      customerIdentification = customerIdentification,
      tradingNames = getTradingNames(answers),
      intermediaryDetails = None,
      otherAddress = getOtherAddress(customerIdentification.idType, answers),
      schemeDetails = getSchemeDetails(answers, commencementDate),
      bankDetails = None
    )
  }

  private def getCustomerIdentification(answers: UserAnswers): EtmpCustomerIdentification = {
    answers.get(IntermediaryDetailsQuery).flatMap { intermediaryDetails =>
      answers.get(ClientHasVatNumberPage).flatMap {
        case true =>
          val vatNumber = answers.get(ClientVatNumberPage).getOrElse {
            val exception = new IllegalStateException("Must have a VAT number if said yes to having a VAT number")
            logger.error(exception.getMessage, exception)
            throw exception
          }
          Some(EtmpCustomerIdentification(EtmpIdType.VRN, vatNumber, intermediaryDetails.intermediaryNumber))
        case false =>
          answers.get(BusinessBasedInUKPage).flatMap {
            case true =>
              answers.get(ClientHasUtrNumberPage).map {
                case true =>
                  val utrNumber = answers.get(ClientUtrNumberPage).getOrElse {
                    val exception = new IllegalStateException("Must have a UTR number if said yes to having a UTR number")
                    logger.error(exception.getMessage, exception)
                    throw exception
                  }
                  EtmpCustomerIdentification(EtmpIdType.UTR, utrNumber, intermediaryDetails.intermediaryNumber)
                case false =>
                  val ninoNumber = answers.get(ClientsNinoNumberPage).getOrElse {
                    val exception = new IllegalStateException("Must have a Nino number if said no to having a UTR number")
                    logger.error(exception.getMessage, exception)
                    throw exception
                  }
                  EtmpCustomerIdentification(EtmpIdType.NINO, ninoNumber, intermediaryDetails.intermediaryNumber)
              }
            case false =>
              val ftrNumber = answers.get(ClientTaxReferencePage).getOrElse {
                val exception = new IllegalStateException("Must have a FTR number if said no to having a UK VAT number and not based in UK")
                logger.error(exception.getMessage, exception)
                throw exception
              }
              Some(EtmpCustomerIdentification(EtmpIdType.FTR, ftrNumber, intermediaryDetails.intermediaryNumber))
          }
      }
    }.getOrElse {
      val exception = new IllegalStateException("Must have answered on having a UK VAT number and have intermediary details")
      logger.error(exception.getMessage, exception)
      throw exception
    }
  }
}
