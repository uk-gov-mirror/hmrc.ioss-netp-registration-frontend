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

package utils

import base.SpecBase
import models.{ClientBusinessName, Country, InternationalAddress}
import models.requests.DataRequest
import org.mockito.Mockito.when
import org.scalacheck.Gen
import org.scalatestplus.mockito.MockitoSugar
import pages.*
import play.api.mvc.AnyContent
import play.api.mvc.Results.Redirect
import play.api.test.Helpers.*

class VatInfoCompletionChecksSpec extends SpecBase with MockitoSugar {

  private val vatInfoCompletionChecks: VatInfoCompletionChecks.type = VatInfoCompletionChecks
  private val validAnswersUkBased = emptyUserAnswersWithVatInfo
    .set(BusinessBasedInUKPage, true).success.value
  
  private val clientBusinessName: ClientBusinessName = ClientBusinessName(vatCustomerInfo.organisationName.value)
  private val countries: Seq[Country] = Gen.listOf(arbitraryCountry.arbitrary).sample.value
  private val country: Country = Gen.oneOf(countries).sample.value
  private val businessAddress: InternationalAddress = InternationalAddress(
    line1 = "line-1",
    line2 = None,
    townOrCity = "town-or-city",
    stateOrRegion = None,
    postCode = None,
    country = Some(country)
  )

  "VatInfoCompletionChecks" - {

    ".isBasedInUk" - {

      "must return true when answers for the section are defined" in {
        val application = applicationBuilder(userAnswers = Some(validAnswersUkBased)).build()

        running(application) {

          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn validAnswersUkBased

          val result = vatInfoCompletionChecks.isBasedInUk()

          result mustBe true
        }
      }
    }

    ".incompleteBusinessBasedInUkRedirect" - {

      "must redirect to the correct page when BusinessBasedInUKPage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteBusinessBasedInUkRedirect(waypoints)

          result mustBe Some(Redirect(controllers.routes.BusinessBasedInUKController.onPageLoad(waypoints)))
        }
      }

      "must return None when BusinessBasedInUKPage is defined" in {
        val userAnswers = validAnswersUkBased

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteBusinessBasedInUkRedirect(waypoints)

          result mustBe None
        }

      }
    }

    ".hasUkVatNumber" - {

      "must return true when BusinessBasedInUKPage is false" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.hasUkVatNumber()
          result mustBe true
        }
      }

      "must return true when BusinessBasedInUKPage is true and ClientHasVatNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, true).success.value
          .set(ClientVatNumberPage, vatNumber).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.hasUkVatNumber()
          result mustBe true
        }
      }

      "must return false when BusinessBasedInUKPage is true and ClientHasVatNumberPage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.hasUkVatNumber()
          result mustBe false
        }
      }

      "must return false when BusinessBasedInUKPage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.hasUkVatNumber()
          result mustBe false
        }
      }
    }

    ".incompleteHasVatNumberRedirect" - {

      "must Redirect to ClientHasVatNumberPage when ClientHasVatNumber is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteHasVatNumberRedirect(waypoints)

          result mustBe Some(Redirect(controllers.routes.ClientHasVatNumberController.onPageLoad(waypoints)))
        }
      }

      "must return None when BusinessBasedInUKPage is false" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteHasVatNumberRedirect(waypoints)

          result mustBe None
        }
      }

      "must return None when ClientHasVatNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteHasVatNumberRedirect(waypoints)

          result mustBe None
        }
      }
    }

    ".ukVatNumberDefined" - {

      "must return true when ClientVatNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, true).success.value
          .set(ClientVatNumberPage, vatNumber).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.ukVatNumberDefined()
          result mustBe true
        }
      }

      "must return false when ClientVatNumberPage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.ukVatNumberDefined()
          result mustBe false
        }
      }
    }

    ".incompleteClientVatNumberRedirect" - {

      "must Redirect to ClientVatNumberPage when ClientVatNumber is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientVatNumberRedirect(waypoints)

          result mustBe Some(Redirect(controllers.routes.ClientVatNumberController.onPageLoad(waypoints)))
        }
      }

      "must return None when ClientHasVatNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, true).success.value
          .set(ClientVatNumberPage, vatNumber).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientVatNumberRedirect(waypoints)

          result mustBe None
        }
      }
    }

    ".hasUtrNumber" - {

      "must return true when ClientHasVatNumberPage is false, and ClientHasUtrNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientHasUtrNumberPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.hasUtrNumber()
          result mustBe true
        }
      }

      "must return false when ClientHasVatNumberPage is false, and ClientHasUtrNumberPage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value


        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.hasUtrNumber()
          result mustBe false
        }
      }

      "must return false when ClientHasVatNumberPage is not defined" in {

        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()
        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.hasUtrNumber()
          result mustBe false
        }

      }
    }

    ".incompleteHasUtrNumberRedirect" - {

      "must Redirect to ClientHasUtrNumberPage when ClientHasUtrNumber is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteHasUtrNumberRedirect(waypoints)

          result mustBe Some(Redirect(controllers.routes.ClientHasUtrNumberController.onPageLoad(waypoints)))
        }
      }

      "must return None when ClientHasUtrNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientHasUtrNumberPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteHasUtrNumberRedirect(waypoints)

          result mustBe None
        }
      }
    }

    ".utrNumberDefined" - {

      "must return true when ClientUtrNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, true).success.value
          .set(ClientHasUtrNumberPage, true).success.value
          .set(ClientUtrNumberPage, utr).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.utrNumberDefined()
          result mustBe true
        }
      }

      "must return false when ClientUtrNumberPage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientHasUtrNumberPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.utrNumberDefined()
          result mustBe false
        }
      }
    }

    ".incompleteClientUtrNumberRedirect" - {

      "must Redirect to ClientUtrNumberPage when ClientUtrNumber is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientHasUtrNumberPage, true).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientUtrNumberRedirect(waypoints)

          result mustBe Some(Redirect(controllers.routes.ClientUtrNumberController.onPageLoad(waypoints)))
        }
      }

      "must return None when ClientUtrNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientHasUtrNumberPage, true).success.value
          .set(ClientUtrNumberPage, utr).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientUtrNumberRedirect(waypoints)

          result mustBe None
        }
      }
    }

    ".ninoNumberDefined" - {

      "must return true when ClientSNinoNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, true).success.value
          .set(ClientHasUtrNumberPage, false).success.value
          .set(ClientUtrNumberPage, nino).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.ninoNumberDefined()
          result mustBe true
        }
      }

      "must return false when ClientUtrNumberPage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientHasUtrNumberPage, false).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.ninoNumberDefined()
          result mustBe false
        }
      }
    }

    ".incompleteClientsNinoNumberRedirect" - {

      "must Redirect to ClientsNinoNumberPage when ClientsNinoNumber is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientHasUtrNumberPage, false).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientsNinoNumberRedirect(waypoints)

          result mustBe Some(Redirect(controllers.routes.ClientsNinoNumberController.onPageLoad(waypoints)))
        }
      }

      "must return None when ClientUtrNumberPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, true).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientHasUtrNumberPage, false).success.value
          .set(ClientsNinoNumberPage, nino).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientsNinoNumberRedirect(waypoints)

          result mustBe None
        }
      }
    }

    ".clientBusinessAddressDefined" - {

      "BusinessBasedInUK is true" - {

        "must return true when ClientBusinessAddressDefined is defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, true).success.value
            .set(ClientHasVatNumberPage, false).success.value
            .set(ClientHasUtrNumberPage, true).success.value
            .set(ClientUtrNumberPage, utr).success.value
            .set(ClientBusinessAddressPage, businessAddress).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.clientBusinessAddressDefined()
            result mustBe true
          }
        }

        "must return false when ClientBusinessAddressDefined is not defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, true).success.value
            .set(ClientHasVatNumberPage, false).success.value
            .set(ClientHasUtrNumberPage, true).success.value
            .set(ClientUtrNumberPage, utr).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.clientBusinessAddressDefined()
            result mustBe false
          }
        }
      }

      "BusinessBasedInUK is false" - {

        "must return true when ClientBusinessAddressDefined is defined" in {

          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, false).success.value
            .set(ClientCountryBasedPage, country).success.value
            .set(ClientBusinessNamePage, clientBusinessName).success.value
            .set(ClientTaxReferencePage, taxReference).success.value
            .set(ClientBusinessAddressPage, businessAddress).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.clientBusinessAddressDefined()
            result mustBe true
          }
        }

        "must return false when ClientBusinessAddressDefined is not defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, false).success.value
            .set(ClientCountryBasedPage, country).success.value
            .set(ClientBusinessNamePage, clientBusinessName).success.value
            .set(ClientTaxReferencePage, taxReference).success.value
            .set(ClientUtrNumberPage, utr).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.clientBusinessAddressDefined()
            result mustBe false
          }
        }
      }
    }

    ".incompleteBusinessAddressRedirect" - {

      "BusinessBasedInUK is true" - {

        "must Redirect to ClientBusinessAddressPage when ClientBusinessAddress is not defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, true).success.value
            .set(ClientHasVatNumberPage, false).success.value
            .set(ClientHasUtrNumberPage, true).success.value
            .set(ClientUtrNumberPage, utr).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.incompleteBusinessAddressRedirect(waypoints)

            result mustBe Some(Redirect(controllers.routes.ClientBusinessAddressController.onPageLoad(waypoints)))
          }
        }

        "must return None when ClientBusinessAddressPage is defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, true).success.value
            .set(ClientHasVatNumberPage, false).success.value
            .set(ClientHasUtrNumberPage, false).success.value
            .set(ClientsNinoNumberPage, nino).success.value
            .set(ClientBusinessAddressPage, businessAddress).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.incompleteBusinessAddressRedirect(waypoints)

            result mustBe None
          }
        }
      }

      "BusinessBasedInUK is false" - {

        "must Redirect to ClientBusinessAddressPage when ClientBusinessAddress is not defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, false).success.value
            .set(ClientCountryBasedPage, country).success.value
            .set(ClientBusinessNamePage, clientBusinessName).success.value
            .set(ClientTaxReferencePage, taxReference).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.incompleteBusinessAddressRedirect(waypoints)

            result mustBe Some(Redirect(controllers.routes.ClientBusinessAddressController.onPageLoad(waypoints)))
          }
        }

        "must return None when ClientBusinessAddressPage is defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, false).success.value
            .set(ClientCountryBasedPage, country).success.value
            .set(ClientBusinessNamePage, clientBusinessName).success.value
            .set(ClientTaxReferencePage, taxReference).success.value
            .set(ClientBusinessAddressPage, businessAddress).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.incompleteBusinessAddressRedirect(waypoints)

            result mustBe None
          }
        }
      }
    }

    ".hasClientBusinessName" - {

      "BusinessBasedInUK is true" - {

        "must return true when ClientBusinessNamePage is defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, true).success.value
            .set(ClientHasVatNumberPage, false).success.value
            .set(ClientBusinessNamePage, clientBusinessName).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.hasClientBusinessName()
            result mustBe true
          }
        }

        "must return false when ClientBusinessNamePage is not defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, true).success.value
            .set(ClientHasVatNumberPage, false).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.hasClientBusinessName()
            result mustBe false
          }
        }
      }

      "BusinessBasedInUK is false" - {

        "must return true when ClientBusinessNamePage is defined" in {

          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, false).success.value
            .set(ClientCountryBasedPage, country).success.value
            .set(ClientBusinessNamePage, clientBusinessName).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.hasClientBusinessName()
            result mustBe true
          }
        }

        "must return false when ClientBusinessNamePage is not defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, false).success.value
            .set(ClientCountryBasedPage, country).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.hasClientBusinessName()
            result mustBe false
          }
        }
      }

    }

    ".incompleteClientBusinessNameRedirect" - {

      "BusinessBasedInUK is true" - {

        "must Redirect to ClientBusinessNamePage when ClientBusinessName is not defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, true).success.value
            .set(ClientHasVatNumberPage, false).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.incompleteClientBusinessNameRedirect(waypoints)

            result mustBe Some(Redirect(controllers.routes.ClientBusinessNameController.onPageLoad(waypoints)))
          }
        }

        "must return None when ClientBusinessNamePage is defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, true).success.value
            .set(ClientHasVatNumberPage, false).success.value
            .set(ClientBusinessNamePage, clientBusinessName).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.incompleteClientBusinessNameRedirect(waypoints)

            result mustBe None
          }
        }
      }

      "BusinessBasedInUK is false" - {

        "must Redirect to ClientBusinessNamePage when ClientBusinessName is not defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, false).success.value
            .set(ClientCountryBasedPage, country).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.incompleteClientBusinessNameRedirect(waypoints)

            result mustBe Some(Redirect(controllers.routes.ClientBusinessNameController.onPageLoad(waypoints)))
          }
        }

        "must return None when ClientBusinessNamePage is defined" in {
          val userAnswers = emptyUserAnswersWithVatInfo
            .set(BusinessBasedInUKPage, false).success.value
            .set(ClientCountryBasedPage, country).success.value
            .set(ClientBusinessNamePage, clientBusinessName).success.value

          val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

          running(application) {
            implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
            when(request.userAnswers) thenReturn userAnswers

            val result = vatInfoCompletionChecks.incompleteClientBusinessNameRedirect(waypoints)

            result mustBe None
          }
        }
      }

    }

    ".clientCountryBasedDefined" - {

      "must return true when ClientCountryBasedPage is defined" in {

        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value
          .set(ClientCountryBasedPage, country).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.clientCountryBasedDefined()
          result mustBe true
        }
      }

      "must return false when ClientBusinessNamePage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.clientCountryBasedDefined()
          result mustBe false
        }
      }
    }

    ".incompleteClientCountryRedirect" - {

      "must Redirect to ClientCountryBasedPage when ClientCountryBased is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientCountryRedirect(waypoints)

          result mustBe Some(Redirect(controllers.routes.ClientCountryBasedController.onPageLoad(waypoints)))
        }
      }

      "must return None when ClientCountryBasedPage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value
          .set(ClientCountryBasedPage, country).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientCountryRedirect(waypoints)

          result mustBe None
        }
      }

    }

    ".clientTaxReferenceDefined" - {

      "must return true when ClientTaxReferencePage is defined" in {

        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientCountryBasedPage, country).success.value
          .set(ClientBusinessNamePage, clientBusinessName).success.value
          .set(ClientTaxReferencePage, taxReference).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.clientTaxReferenceDefined()
          result mustBe true
        }
      }

      "must return false when ClientTaxReferencePage is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value
          .set(ClientCountryBasedPage, country).success.value
          .set(ClientBusinessNamePage, clientBusinessName).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.clientTaxReferenceDefined()
          result mustBe false
        }
      }

    }

    ".incompleteClientTaxReferenceRedirect" - {

      "must Redirect to ClientTaxReferencePage when ClientTaxReference is not defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value
          .set(ClientHasVatNumberPage, false).success.value
          .set(ClientCountryBasedPage, country).success.value
          .set(ClientBusinessNamePage, clientBusinessName).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientTaxReferenceRedirect(waypoints)

          result mustBe Some(Redirect(controllers.routes.ClientTaxReferenceController.onPageLoad(waypoints)))
        }
      }

      "must return None when ClientTaxReferencePage is defined" in {
        val userAnswers = emptyUserAnswersWithVatInfo
          .set(BusinessBasedInUKPage, false).success.value
          .set(ClientCountryBasedPage, country).success.value
          .set(ClientBusinessNamePage, clientBusinessName).success.value
          .set(ClientTaxReferencePage, taxReference).success.value

        val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

        running(application) {
          implicit val request: DataRequest[AnyContent] = mock[DataRequest[AnyContent]]
          when(request.userAnswers) thenReturn userAnswers

          val result = vatInfoCompletionChecks.incompleteClientTaxReferenceRedirect(waypoints)

          result mustBe None
        }
      }
    }
  }
}
