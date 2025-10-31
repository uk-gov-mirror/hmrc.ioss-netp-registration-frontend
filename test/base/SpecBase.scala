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

package base

import controllers.actions.*
import generators.Generators
import models.domain.VatCustomerInfo
import models.etmp.display.RegistrationWrapper
import models.{BusinessContactDetails, CheckMode, Index, IntermediaryDetails, UserAnswers, Website}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{OptionValues, TryValues}
import pages.*
import pages.previousRegistrations.*
import pages.tradingNames.*
import pages.vatEuDetails.*
import pages.website.*
import play.api.Application
import play.api.i18n.{Messages, MessagesApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.AnyContentAsEmpty
import play.api.test.CSRFTokenHelper.CSRFRequest
import play.api.test.FakeRequest
import queries.IntermediaryDetailsQuery
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.domain.Vrn

import java.time.{Clock, Instant, LocalDate, ZoneId}
import java.util.UUID

trait SpecBase
  extends AnyFreeSpec
    with Matchers
    with TryValues
    with OptionValues
    with ScalaFutures
    with IntegrationPatience
    with Generators {

  val userAnswersId: String = "12345-credId"
  val intermediaryDetails: IntermediaryDetails = IntermediaryDetails("IN9001234567", "Intermediary Name")

  def countryIndex(index: Int): Index = Index(index)

  val journeyId: String = UUID.randomUUID().toString

  val arbitraryInstant: Instant = arbitraryDate.arbitrary.sample.value.atStartOfDay(ZoneId.systemDefault()).toInstant
  val stubClockAtArbitraryDate: Clock = Clock.fixed(arbitraryInstant, ZoneId.systemDefault())

  val waypoints: Waypoints = EmptyWaypoints

  lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest("", "/endpoint").withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  def messages(app: Application): Messages = app.injector.instanceOf[MessagesApi].preferred(FakeRequest())

  def emptyUserAnswers: UserAnswers = UserAnswers(id = userAnswersId, journeyId = journeyId, lastUpdated = arbitraryInstant)

  def emptyUserAnswersWithVatInfo: UserAnswers = emptyUserAnswers.copy(vatInfo = Some(vatCustomerInfo))

  def basicUserAnswersWithVatInfo: UserAnswers = emptyUserAnswers
    .set(BusinessBasedInUKPage, true).success.value
    .set(ClientHasVatNumberPage, true).success.value
    .set(ClientVatNumberPage, "123456789").success.value
    .set(HasTradingNamePage, false).success.value
    .set(BusinessContactDetailsPage, BusinessContactDetails("fullName", "555999111", "test@test.com")).success.value
    .set(HasFixedEstablishmentPage, false).success.value
    .set(PreviouslyRegisteredPage, false).success.value
    .set(WebsitePage(Index(0)), Website("www.website.com")).success.value
    .set(IntermediaryDetailsQuery, intermediaryDetails).success.value
    .copy(vatInfo = Some(vatCustomerInfo))

  def testCredentials: Credentials = Credentials(userAnswersId, "GGW")

  val vatNumber = "GB123456789"
  val intermediaryNumber = "IN9001234567"
  val iossNumber = "IM9001234568"
  val intermediaryName = "Intermediary Company Name"
  val vrn: Vrn = Vrn("123456789")
  val utr: String = "1234567890"
  val nino = "QQ 12 34 56 C"
  val taxReference: String = "123456789"
  val vatCustomerInfo: VatCustomerInfo = {
    VatCustomerInfo(
      registrationDate = LocalDate.now(stubClockAtArbitraryDate),
      desAddress = arbitraryDesAddress.arbitrary.sample.value,
      organisationName = Some("Company name"),
      individualName = None,
      singleMarketIndicator = true,
      deregistrationDecisionDate = None
    )
  }

  val intermediaryVatCustomerInfo: VatCustomerInfo = {
    VatCustomerInfo(
      registrationDate = LocalDate.now(stubClockAtArbitraryDate),
      desAddress = arbitraryDesAddress.arbitrary.sample.value,
      organisationName = Some(intermediaryDetails.intermediaryName),
      individualName = None,
      singleMarketIndicator = true,
      deregistrationDecisionDate = None
    )
  }

  val businessContactDetails: BusinessContactDetails =
    BusinessContactDetails(fullName = "name", telephoneNumber = "0111 2223334", emailAddress = "email@example.com")

  val registrationWrapper: RegistrationWrapper = arbitraryRegistrationWrapper.arbitrary.sample.value

  protected def applicationBuilder(
                                    userAnswers: Option[UserAnswers] = None,
                                    clock: Option[Clock] = None,
                                    intermediaryNumber: Option[String] = None,
                                    iossNumber: Option[String] = None,
                                    registrationWrapper: Option[RegistrationWrapper] = None
                                  ): GuiceApplicationBuilder = {

    val clockToBind = clock.getOrElse(stubClockAtArbitraryDate)
    new GuiceApplicationBuilder()
      .overrides(
        bind[IdentifierAction].to[FakeIdentifierAction],
        bind[DataRetrievalAction].toInstance(new FakeDataRetrievalAction(userAnswers)),
        bind[ClientDataRetrievalAction].toInstance(new FakeClientDataRetrievalAction(userAnswers)),
        bind[ClientIdentifierAction].to[FakeClientIdentifierAction],
        bind[ClientValidationFilterProvider].to[FakeClientValidationFilterProvider],
        bind[ClientDeclarationFilterProvider].to[FakeClientDeclarationFilterProvider],
        bind[DataRequiredAction].toInstance(new FakeDataRequiredAction(userAnswers, intermediaryNumber.getOrElse(this.intermediaryNumber), registrationWrapper.getOrElse(this.registrationWrapper))),
        bind[RegistrationRequiredAction].toInstance(new FakeRegistrationRequiredAction(userAnswers, iossNumber.getOrElse(this.iossNumber), registrationWrapper.getOrElse(this.registrationWrapper))),
        bind[Clock].toInstance(clockToBind)
      )
  }

  protected def createCheckModeWayPoint(checkAnswersPage: CheckAnswersPage): NonEmptyWaypoints =
    EmptyWaypoints.setNextWaypoint(Waypoint(checkAnswersPage, CheckMode, checkAnswersPage.urlFragment))
}
