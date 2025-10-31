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

package controllers.actions

import connectors.RegistrationConnector
import connectors.RegistrationHttpParser.EtmpDisplayRegistrationResponse
import controllers.routes
import logging.Logging
import models.etmp.display.RegistrationWrapper
import models.requests.{DataRequest, OptionalDataRequest}
import models.responses.ErrorResponse
import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionRefiner, Result}
import queries.IossNumberQuery
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class DataRequiredActionImpl @Inject()(
                                        registrationConnector: RegistrationConnector,
                                        isInAmendMode: Boolean
                                      )(implicit val executionContext: ExecutionContext)
  extends ActionRefiner[OptionalDataRequest, DataRequest] with Logging {

  override protected def refine[A](request: OptionalDataRequest[A]): Future[Either[Result, DataRequest[A]]] = {

    val intermediaryNumber = request.intermediaryNumber.getOrElse {
      logger.warn(s"The intermediary number is required for ${request.userId} to complete journey")
      throw new IllegalStateException(s"The Intermediary Number is required for ${request.userId}")
    }

    request.userAnswers match {
      case None =>
        Future.successful(Left(Redirect(routes.JourneyRecoveryController.onPageLoad())))

      case Some(data) =>
        val eventualMaybeRegistrationWrapper: Future[Option[RegistrationWrapper]] = {
          if (isInAmendMode) {
            implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request.request, request.session)
            val registrationFuture: Future[EtmpDisplayRegistrationResponse] = data.get(IossNumberQuery) match {
              case Some(iossNumber) =>
                logger.info(s"Fetching NETP client registration: $iossNumber")
                registrationConnector.displayRegistrationNetp(iossNumber)(hc)

              case None =>
                logger.info(s"Fetching intermediary registration: $intermediaryNumber")
                registrationConnector.displayRegistrationIntermediary(intermediaryNumber)(hc)
            }

            registrationFuture.flatMap {
              case Left(error: ErrorResponse) =>
                Future.failed(new RuntimeException(s"Failed to retrieve registration whilst in amend mode: ${error.body}"))
              case Right(registrationWrapper: RegistrationWrapper) =>
                Future.successful(Some(registrationWrapper))
            }
          } else {
            Future.successful(None)
          }
        }

        eventualMaybeRegistrationWrapper.map { maybeWrapper =>
          Right(DataRequest(
            request = request.request,
            userId = request.userId,
            userAnswers = data,
            intermediaryNumber = intermediaryNumber,
            iossNumber = data.get(IossNumberQuery),
            registrationWrapper = maybeWrapper
          ))
        }
    }
  }
}

class DataRequiredAction @Inject()(
                                    registrationConnector: RegistrationConnector
                                  )(implicit ec: ExecutionContext) {

  def apply(isInAmendMode: Boolean = false): ActionRefiner[OptionalDataRequest, DataRequest] = {
    new DataRequiredActionImpl(registrationConnector, isInAmendMode)
  }
}
