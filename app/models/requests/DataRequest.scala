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

package models.requests

import models.UserAnswers
import models.etmp.display.RegistrationWrapper
import play.api.mvc.{Request, WrappedRequest}

sealed abstract class GenericRequest[+A](
                                          request: Request[A],
                                          val userId: String,
                                          val userAnswers: UserAnswers
                                        ) extends WrappedRequest[A](request)

case class OptionalDataRequest[A](
                                   request: Request[A],
                                   userId: String,
                                   userAnswers: Option[UserAnswers] = None,
                                   intermediaryNumber: Option[String] = None
                                 ) extends WrappedRequest[A](request)

case class DataRequest[A](
                           request: Request[A],
                           override val userId: String,
                           override val userAnswers: UserAnswers,
                           intermediaryNumber: String,
                           iossNumber: Option[String],
                           registrationWrapper: Option[RegistrationWrapper] = None
                         ) extends GenericRequest[A](request, userId, userAnswers)

// TODO -> Required???
case class IntermediaryDataRequest[A](
                                       request: Request[A],
                                       override val userId: String,
                                       override val userAnswers: UserAnswers,
                                       intermediaryNumber: String,
                                       registrationWrapper: RegistrationWrapper
                                     ) extends GenericRequest[A](request, userId, userAnswers)

case class ClientOptionalDataRequest[A](
                                         request: Request[A],
                                         override val userId: String,
                                         override val userAnswers: UserAnswers
                                       ) extends GenericRequest[A](request, userId, userAnswers)
