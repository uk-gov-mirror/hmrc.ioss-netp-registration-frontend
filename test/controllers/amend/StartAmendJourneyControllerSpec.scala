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

package controllers.amend

import base.SpecBase
import connectors.RegistrationConnector
import models.etmp.display.RegistrationWrapper
import models.responses.NotFound
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.*
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar
import play.api.http.Status.SEE_OTHER
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.{GET, defaultAwaitTimeout, route, running, status, writeableOf_AnyContentAsEmpty}
import services.RegistrationService
import utils.FutureSyntax.FutureOps

import scala.concurrent.Future

class StartAmendJourneyControllerSpec extends SpecBase with MockitoSugar with BeforeAndAfterEach {

  private val mockRegistrationConnector: RegistrationConnector = mock[RegistrationConnector]
  private val mockRegistrationService: RegistrationService = mock[RegistrationService]

  private val registrationWrapper: RegistrationWrapper = arbitraryRegistrationWrapper.arbitrary.sample.value

  override def beforeEach(): Unit =
    reset(
      mockRegistrationConnector,
      mockRegistrationService
    )
  
  "StartAmendJourneyController" - {

    "must return OK" in {

      when(mockRegistrationConnector.displayRegistrationNetp(any())(any())) thenReturn Right(registrationWrapper).toFuture
      when(mockRegistrationService.toUserAnswers(any(), any())) thenReturn basicUserAnswersWithVatInfo.toFuture

      val application = applicationBuilder(userAnswers = Some(basicUserAnswersWithVatInfo))
        .overrides(bind[RegistrationConnector].toInstance(mockRegistrationConnector))
        .overrides(bind[RegistrationService].toInstance(mockRegistrationService))
        .build()

      running(application) {
        val request = FakeRequest(GET, controllers.amend.routes.StartAmendJourneyController.onPageLoad(iossNumber).url)

        val result = route(application, request).value

        status(result) mustBe SEE_OTHER
        verify(mockRegistrationConnector, times(1)).displayRegistrationNetp(eqTo(iossNumber))(any())
        verify(mockRegistrationService, times(1)).toUserAnswers(eqTo(userAnswersId), eqTo(registrationWrapper))
      }
    }

    "must throw an Exception when the RegistrationConnector throws an error" in {

      when(mockRegistrationConnector.displayRegistrationNetp(any())(any())) thenReturn Left(NotFound).toFuture

      val application = applicationBuilder(userAnswers = Some(basicUserAnswersWithVatInfo))
        .overrides(bind[RegistrationConnector].toInstance(mockRegistrationConnector))
        .build()

      running(application) {
        val request = FakeRequest(GET, controllers.amend.routes.StartAmendJourneyController.onPageLoad(iossNumber).url)

        val result = route(application, request).value

        whenReady(result.failed) { exp =>
          exp `mustBe` a[Exception]
          exp.getMessage `mustBe` NotFound.body
        }
        verify(mockRegistrationConnector, times(1)).displayRegistrationNetp(eqTo(iossNumber))(any())
        verifyNoInteractions(mockRegistrationService)
      }
    }

    "must throw an Exception when the RegistrationService.toUserAnswers call fails to convert UserAnswers" in {

      val errorMessage: String = "ERROR"

      when(mockRegistrationConnector.displayRegistrationNetp(any())(any())) thenReturn Right(registrationWrapper).toFuture
      when(mockRegistrationService.toUserAnswers(any(), any())) thenReturn Future.failed(Exception(errorMessage))

      val application = applicationBuilder(userAnswers = Some(basicUserAnswersWithVatInfo))
        .overrides(bind[RegistrationConnector].toInstance(mockRegistrationConnector))
        .overrides(bind[RegistrationService].toInstance(mockRegistrationService))
        .build()

      running(application) {
        val request = FakeRequest(GET, controllers.amend.routes.StartAmendJourneyController.onPageLoad(iossNumber).url)

        val result = route(application, request).value

        whenReady(result.failed) { exp =>
          exp `mustBe` a[Exception]
          exp.getMessage `mustBe` errorMessage
        }
        verify(mockRegistrationConnector, times(1)).displayRegistrationNetp(eqTo(iossNumber))(any())
        verify(mockRegistrationService, times(1)).toUserAnswers(eqTo(userAnswersId), eqTo(registrationWrapper))
      }
    }
  }
}
