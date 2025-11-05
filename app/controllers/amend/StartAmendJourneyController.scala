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

import connectors.RegistrationConnector
import controllers.actions.AuthenticatedControllerComponents
import logging.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.{IossNumberQuery, OriginalRegistrationQuery}
import services.RegistrationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class StartAmendJourneyController @Inject()(
                                             override val messagesApi: MessagesApi,
                                             cc: AuthenticatedControllerComponents,
                                             registrationConnector: RegistrationConnector,
                                             registrationService: RegistrationService,
                                             val controllerComponents: MessagesControllerComponents
                                           )(implicit ec: ExecutionContext)
  extends FrontendBaseController with I18nSupport with Logging {

  def onPageLoad(iossNumber: String): Action[AnyContent] = cc.identifyAndGetOptionalData.async {
    implicit request =>

      (for {
        displayRegistrationResponse <- registrationConnector.displayRegistrationNetp(iossNumber)
      } yield {

        displayRegistrationResponse match {
          case Right(registrationWrapper) =>

            for {
              userAnswers <- registrationService.toUserAnswers(request.userId, registrationWrapper)
              answersWithIossNumber <- Future.fromTry(userAnswers.set(IossNumberQuery, iossNumber))
              originalAnswers <- Future.fromTry(
                answersWithIossNumber.set(OriginalRegistrationQuery(iossNumber), registrationWrapper.etmpDisplayRegistration))
              _ <- cc.sessionRepository.set(userAnswers)
              _ <- cc.sessionRepository.set(originalAnswers)
            } yield {

              Redirect(routes.ChangeRegistrationController.onPageLoad(waypoints).url)
            }
          case Left(error) =>
            val exception = new Exception(error.body)
            logger.error(exception.getMessage, exception)
            throw exception
        }
      }).flatten
  }
}
