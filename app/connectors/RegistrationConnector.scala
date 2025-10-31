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

package connectors

import config.Service
import connectors.RegistrationHttpParser.*
import connectors.SavedPendingRegistrationHttpParser.{SavedPendingRegistrationResponse, SavedPendingRegistrationResultResponseReads}
import connectors.SavedPendingRegistrationsHttpParser.*
import connectors.ValidateClientCodeHttpParser.{ValidateClientCodeReads, validateClientCodeResponse}
import connectors.VatCustomerInfoHttpParser.{VatCustomerInfoResponse, VatCustomerInfoResponseReads}
import logging.Logging
import models.PendingRegistrationRequest
import models.etmp.EtmpRegistrationRequest
import models.etmp.amend.EtmpAmendRegistrationRequest
import play.api.Configuration
import play.api.libs.json.Json
import play.api.libs.ws.writeableOf_JsValue
import uk.gov.hmrc.domain.Vrn
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpErrorFunctions, StringContextOps}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class RegistrationConnector @Inject()(config: Configuration, httpClientV2: HttpClientV2)
                                     (implicit executionContext: ExecutionContext) extends HttpErrorFunctions with Logging {

  private val baseUrl: Service = config.get[Service]("microservice.services.ioss-netp-registration")
  private val intermediaryUrl: Service = config.get[Service]("microservice.services.ioss-intermediary-registration")
  private val iossUrl: Service = config.get[Service]("microservice.services.ioss-registration")
  private val ossUrl: Service = config.get[Service]("microservice.services.one-stop-shop-registration")

  def getVatCustomerInfo(ukVatNumber: String)(implicit hc: HeaderCarrier): Future[VatCustomerInfoResponse] = {
    httpClientV2.get(url"$baseUrl/vat-information/$ukVatNumber").execute[VatCustomerInfoResponse]
  }

  def getIntermediaryVatCustomerInfo()(implicit hc: HeaderCarrier): Future[VatCustomerInfoResponse] = {
    httpClientV2.get(url"$intermediaryUrl/vat-information").execute[VatCustomerInfoResponse]
  }

  def amendRegistration(amendRegistrationRequest: EtmpAmendRegistrationRequest)(implicit hc: HeaderCarrier): Future[AmendRegistrationResultResponse] = {
    httpClientV2.post(url"$baseUrl/amend").withBody(Json.toJson(amendRegistrationRequest)).execute[AmendRegistrationResultResponse]
  }

  def submitPendingRegistration(pendingRegistrationRequest: PendingRegistrationRequest)(implicit hc: HeaderCarrier)
  : Future[SavedPendingRegistrationResponse] = {
    httpClientV2.post(url"$baseUrl/save-pending-registration")
      .withBody(Json.toJson(pendingRegistrationRequest))
      .execute[SavedPendingRegistrationResponse]
  }

  def getPendingRegistration(journeyIdOrUrlCode: String)(implicit hc: HeaderCarrier): Future[SavedPendingRegistrationResponse] = {
    httpClientV2.get(url"$baseUrl/save-pending-registration/$journeyIdOrUrlCode")
      .execute[SavedPendingRegistrationResponse]
  }

  def getPendingRegistrationsByIntermediaryNumber(intermediaryNumber: String)(implicit hc: HeaderCarrier): Future[SavedPendingRegistrationsResponse] = {
    httpClientV2.get(url"$baseUrl/pending-registrations/$intermediaryNumber")
      .execute[SavedPendingRegistrationsResponse]
  }
  
  def deletePendingRegistration(journeyId: String)(implicit hc: HeaderCarrier): Future[Boolean] = {
    httpClientV2.delete(url"$baseUrl/pending-registrations/$journeyId").execute[Boolean]
  }

  def validateClientCode(uniqueUrlCode: String, activationCode: String)(implicit hc: HeaderCarrier): Future[validateClientCodeResponse] = {
    httpClientV2.get(url"$baseUrl/validate-pending-registration/$uniqueUrlCode/$activationCode")
      .execute[validateClientCodeResponse]
  }

  def getIossRegistration(iossNumber: String)(implicit hc: HeaderCarrier): Future[IossEtmpDisplayRegistrationResultResponse] = {
    httpClientV2.get(url"$iossUrl/registration-as-intermediary/$iossNumber").execute[IossEtmpDisplayRegistrationResultResponse]
  }

  def getOssRegistration(vrn: Vrn)(implicit hc: HeaderCarrier): Future[OssRegistrationResponse] = {
    httpClientV2.get(url"$ossUrl/registration/$vrn").execute[OssRegistrationResponse]
  }

  def createRegistration(registrationRequest: EtmpRegistrationRequest)(implicit hc: HeaderCarrier): Future[RegistrationResultResponse] =
    httpClientV2.post(url"$baseUrl/create-registration").withBody(Json.toJson(registrationRequest)).execute[RegistrationResultResponse]

  def updateClientEmailAddress(journeyId: String, newEmailAddress: String)(implicit hc: HeaderCarrier): Future[SavedPendingRegistrationResponse] =
    httpClientV2.put(url"$baseUrl/pending-registrations/$journeyId/$newEmailAddress").execute[SavedPendingRegistrationResponse]

  def displayRegistrationNetp(iossNumber: String)(implicit hc: HeaderCarrier): Future[EtmpDisplayRegistrationResponse] =
    httpClientV2.get(url"$baseUrl/registrations/$iossNumber").execute[EtmpDisplayRegistrationResponse]

  def displayRegistrationIntermediary(intermediaryNumber: String)(implicit hc: HeaderCarrier): Future[EtmpDisplayRegistrationResponse] =
    httpClientV2.get(url"$intermediaryUrl/get-registration/$intermediaryNumber").execute[EtmpDisplayRegistrationResponse]
}
