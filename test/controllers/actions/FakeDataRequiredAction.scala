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
import controllers.actions.FakeDataRequiredAction.mockRegistrationConnector
import controllers.routes
import models.UserAnswers
import models.etmp.display.RegistrationWrapper
import models.requests.{DataRequest, OptionalDataRequest}
import org.scalatestplus.mockito.MockitoSugar.mock
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import queries.IossNumberQuery
import utils.FutureSyntax.FutureOps

import scala.concurrent.{ExecutionContext, Future}

case class FakeDataRequiredActionImpl(
                                       dataToReturn: Option[UserAnswers],
                                       intermediaryNumber: String,
                                       registrationWrapper: RegistrationWrapper,
                                       isInAmendMode: Boolean
                                     )
  extends DataRequiredActionImpl(mockRegistrationConnector, isInAmendMode)(ExecutionContext.Implicits.global) {

  override protected def refine[A](request: OptionalDataRequest[A]): Future[Either[Result, DataRequest[A]]] = {
    dataToReturn match {
      case Some(data) =>
        Right(
          DataRequest(
            request = request,
            userId = request.userId,
            userAnswers = data,
            intermediaryNumber = request.intermediaryNumber.getOrElse(intermediaryNumber),
            iossNumber = data.get(IossNumberQuery),
            registrationWrapper = Some(registrationWrapper)
          )
        ).toFuture

      case _ =>
        Left(Redirect(routes.JourneyRecoveryController.onPageLoad())).toFuture
    }
  }
}

class FakeDataRequiredAction(
                              dataToReturn: Option[UserAnswers],
                              intermediaryNumber: String,
                              registrationWrapper: RegistrationWrapper
                            ) extends DataRequiredAction(mock[RegistrationConnector])(ExecutionContext.Implicits.global) {

  override def apply(isInAmendMode: Boolean): DataRequiredActionImpl = {
    FakeDataRequiredActionImpl(dataToReturn, intermediaryNumber, registrationWrapper, isInAmendMode)
  }
}

object FakeDataRequiredAction {

  val mockRegistrationConnector: RegistrationConnector = mock[RegistrationConnector]
}