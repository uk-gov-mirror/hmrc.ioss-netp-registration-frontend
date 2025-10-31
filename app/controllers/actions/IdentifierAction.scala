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

import com.google.inject.Inject
import config.Constants.intermediaryEnrolmentKey
import config.FrontendAppConfig
import controllers.routes
import logging.Logging
import models.requests.{IdentifierRequest, SessionRequest}
import play.api.mvc.*
import play.api.mvc.Results.*
import services.{IntermediaryRegistrationService, UrlBuilderService}
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.retrieve.*
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.domain.Vrn
import uk.gov.hmrc.http.{HeaderCarrier, UnauthorizedException}
import uk.gov.hmrc.play.bootstrap.binders.RedirectUrl.idFunctor
import uk.gov.hmrc.play.bootstrap.binders.{AbsoluteWithHostnameFromAllowlist, OnlyRelative}
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import utils.FutureSyntax.FutureOps

import scala.concurrent.{ExecutionContext, Future}

trait IdentifierAction extends ActionBuilder[IdentifierRequest, AnyContent] with ActionFunction[Request, IdentifierRequest]

class AuthenticatedIdentifierAction @Inject()(
                                               override val authConnector: AuthConnector,
                                               config: FrontendAppConfig,
                                               val parser: BodyParsers.Default,
                                               intermediaryRegistrationService: IntermediaryRegistrationService,
                                               urlBuilderService: UrlBuilderService
                                             )(implicit val executionContext: ExecutionContext)
  extends IdentifierAction with AuthorisedFunctions with Logging {

  private lazy val redirectPolicy = (OnlyRelative | AbsoluteWithHostnameFromAllowlist(config.allowedRedirectUrls: _*))

  override def invokeBlock[A](request: Request[A], block: IdentifierRequest[A] => Future[Result]): Future[Result] = {

    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

    authorised().retrieve(Retrievals.internalId and Retrievals.allEnrolments) {
      case Some(internalId) ~ enrolments =>
        findIntermediaryNumberFromEnrolments(enrolments) match {
          case Some(intermediaryNumber) =>
            val vrn: Vrn = findVrnFromEnrolments(enrolments)
            intermediaryRegistrationService.getIntermediaryRegistration().flatMap {
              case Some(_) =>
                block(IdentifierRequest(request, internalId, enrolments, vrn, intermediaryNumber))
              case None =>
                logger.error(s"No VAT customer info found for VRN: ${vrn.vrn}")
                Future.failed(new IllegalStateException("Missing VAT customer info"))
            }

          case None =>
            Future.successful(Redirect(routes.CannotUseNotAnIntermediaryController.onPageLoad()))
        }
      case _ =>
        throw new UnauthorizedException("Unable to retrieve internal Id")
    } recover {
      case _: NoActiveSession =>
        Redirect(config.loginUrl, Map("continue" -> Seq(urlBuilderService.loginContinueUrl(request).get(redirectPolicy).url)))
      case _: AuthorisationException =>
        Redirect(routes.UnauthorisedController.onPageLoad())
    }
  }

  private def findVrnFromEnrolments(enrolments: Enrolments): Vrn = {
    enrolments.enrolments.find(_.key == "HMRC-MTD-VAT")
      .flatMap(_.identifiers.find(id => id.key == "VRN").map(e => Vrn(e.value)))
      .getOrElse {
        logger.warn("User does not have a valid VAT enrolment")
        throw new IllegalStateException("Missing VAT enrolment")
      }
  }

  private def findIntermediaryNumberFromEnrolments(enrolments: Enrolments): Option[String] = {
    enrolments.enrolments
      .find(_.key == config.intermediaryEnrolment)
      .flatMap(_.identifiers.find(id => id.key == intermediaryEnrolmentKey && id.value.nonEmpty).map(_.value))
  }
}

class SessionIdentifierAction @Inject()()(implicit val executionContext: ExecutionContext)
  extends ActionRefiner[Request, SessionRequest] with ActionFunction[Request, SessionRequest] {

  override def refine[A](request: Request[A]): Future[Either[Result, SessionRequest[A]]] = {

    implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

    hc.sessionId
      .map(session => Right(SessionRequest(request, session.value)).toFuture)
      .getOrElse(Left(Redirect(routes.JourneyRecoveryController.onPageLoad())).toFuture)
  }
}
