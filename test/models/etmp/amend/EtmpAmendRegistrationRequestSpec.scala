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

package models.etmp.amend

import base.SpecBase
import models.etmp.*
import org.scalacheck.Arbitrary.arbitrary
import play.api.libs.json.{JsError, JsSuccess, Json}

class EtmpAmendRegistrationRequestSpec extends SpecBase {

  private val administration = EtmpAdministration(EtmpMessageType.IOSSIntAmendClient)
  private val customerIdentification = EtmpAmendCustomerIdentification(intermediaryNumber)
  private val tradingNames = Seq(EtmpTradingName("Test Trading Name"))
  private val changeLog = arbitrary[EtmpAmendRegistrationChangeLog].sample.value

  private val schemeDetails = arbitraryEtmpSchemeDetails.arbitrary.sample.value
  private val bankDetails = arbitraryEtmpBankDetails.arbitrary.sample.value
  private val otherAddress = Some(arbitraryEtmpOtherAddress.arbitrary.sample.value)

  "EtmpAmendRegistrationRequest" - {

    "must deserialise/serialise to and from EtmpAmendRegistrationRequest" in {

      val json = Json.obj(
        "administration" -> administration,
        "changeLog" -> changeLog,
        "customerIdentification" -> customerIdentification,
        "tradingNames" -> tradingNames,
        "otherAddress" -> otherAddress,
        "schemeDetails" -> schemeDetails,
        "bankDetails" -> Some(bankDetails)
      )

      val expectedResult = EtmpAmendRegistrationRequest(
        administration = administration,
        changeLog = changeLog,
        customerIdentification = customerIdentification,
        tradingNames = tradingNames,
        intermediaryDetails = None,
        otherAddress = otherAddress,
        schemeDetails = schemeDetails,
        bankDetails = Some(bankDetails)
      )

      Json.toJson(expectedResult) mustBe json
      json.validate[EtmpAmendRegistrationRequest] mustBe JsSuccess(expectedResult)
    }

    "must handle missing fields during deserialization" in {
      val json = Json.obj()

      json.validate[EtmpAmendRegistrationRequest] mustBe a[JsError]
    }

    "must handle invalid data during deserialization" in {

      val json = Json.obj(
        "administration" -> administration,
        "changeLog" -> changeLog,
        "customerIdentification" -> customerIdentification,
        "tradingNames" -> 12345,
        "otherAddress" -> otherAddress,
        "schemeDetails" -> schemeDetails,
        "bankDetails" -> bankDetails
      )

      json.validate[EtmpAmendRegistrationRequest] mustBe a[JsError]
    }
  }
}