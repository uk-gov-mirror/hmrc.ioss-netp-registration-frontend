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

import logging.Logging
import models.requests.{AuthenticatedMandatoryRegistrationRequest, DataRequest}
import play.api.mvc.Results.*
import play.api.mvc.{ActionRefiner, Result}
import utils.FutureSyntax.FutureOps

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class RegistrationRequiredActionImpl @Inject()()(implicit val executionContext: ExecutionContext)
  extends ActionRefiner[DataRequest, AuthenticatedMandatoryRegistrationRequest] with Logging {

  override protected def refine[A](request: DataRequest[A]): Future[Either[Result, AuthenticatedMandatoryRegistrationRequest[A]]] = {
    request.iossNumber match {
      case None =>
        val errorMessage: String = "Insufficient IOSS enrolments"
        logger.error(errorMessage)
        val exception = new Exception(errorMessage)
        throw exception

      case Some(iossNumber) =>
        request.registrationWrapper match {
          case Some(registrationWrapper) =>
            Right(
              AuthenticatedMandatoryRegistrationRequest(
                request = request,
                userAnswers = request.userAnswers,
                iossNumber = iossNumber,
                registrationWrapper = registrationWrapper
              )
            ).toFuture

          case _ =>
            val errorMessage: String = "Unable to retrieved a Registration"
            logger.error(errorMessage)
            val exception = new Exception(errorMessage)
            throw exception
        }
    }
  }
}

case class RegistrationRequiredAction @Inject()()(implicit val executionContext: ExecutionContext) {

  def apply(): RegistrationRequiredActionImpl = {
    new RegistrationRequiredActionImpl
  }
}
