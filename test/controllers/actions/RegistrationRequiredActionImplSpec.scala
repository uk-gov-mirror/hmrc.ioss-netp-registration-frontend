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

import base.SpecBase
import models.etmp.display.RegistrationWrapper
import models.requests.{AuthenticatedMandatoryRegistrationRequest, DataRequest}
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.GET

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RegistrationRequiredActionImplSpec extends SpecBase {

  class Harness() extends RegistrationRequiredActionImpl() {

    def callRefine[A](request: DataRequest[A]): Future[Either[Result, AuthenticatedMandatoryRegistrationRequest[A]]] = refine(request)
  }

  "RegistrationRequiredAction" - {

    "must throw an Exception when there are insufficient IOSS enrolments" in {

      val action = new Harness()
      val request = FakeRequest(GET, "/test/url?k=session-id")

      val failedResult = intercept[Exception] {
        action.callRefine(
          DataRequest(
            request = request,
            userId = userAnswersId,
            userAnswers = emptyUserAnswers,
            intermediaryNumber = intermediaryNumber,
            iossNumber = None,
            registrationWrapper = None
          )
        ).futureValue
      }

      failedResult.getMessage must include("Insufficient IOSS enrolments")
    }

    "must throw an Exception when there are sufficient IOSS enrolments but a registration is not retrieved" in {

      val action = new Harness()
      val request = FakeRequest(GET, "/test/url?k=session-id")

      val failedResult = intercept[Exception] {
        action.callRefine(
          DataRequest(
            request = request,
            userId = userAnswersId,
            userAnswers = emptyUserAnswers,
            intermediaryNumber = intermediaryNumber,
            iossNumber = Some(iossNumber),
            registrationWrapper = None
          )
        ).futureValue
      }

      failedResult.getMessage must include("Unable to retrieved a Registration")
    }

    "must return Right AuthenticatedMandatoryIossRequest when there are sufficient IOSS enrolments and a registration is retrieved" in {

      val registrationWrapper: RegistrationWrapper = arbitraryRegistrationWrapper.arbitrary.sample.value

      val request = FakeRequest(GET, "/test/url?k=session-id")
      val dataRequest = DataRequest(
        request = request,
        userId = userAnswersId,
        userAnswers = emptyUserAnswers,
        intermediaryNumber = intermediaryNumber,
        iossNumber = Some(iossNumber),
        registrationWrapper = Some(registrationWrapper)
      )

      val action = new Harness()
      val result = action.callRefine(dataRequest).futureValue

      val expectedResult = AuthenticatedMandatoryRegistrationRequest(
        request = dataRequest,
        userAnswers = dataRequest.userAnswers,
        iossNumber = iossNumber,
        registrationWrapper = registrationWrapper
      )

      result `mustBe` Right(expectedResult)
    }
  }
}
