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

package viewmodels.checkAnswers

import models.{Country, UserAnswers}
import pages.{CheckAnswersPage, ClientCountryBasedPage, Waypoints}
import play.api.i18n.Messages
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.govukfrontend.views.Aliases.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryListRow
import viewmodels.govuk.summarylist.*
import viewmodels.implicits.*


object VatRegistrationDetailsSummary {

  def rowBusinessAddress(
                          waypoints: Waypoints,
                          answers: UserAnswers,
                          sourcePage: CheckAnswersPage)(implicit messages: Messages): Option[SummaryListRow] = {
    answers.vatInfo.map {
      answer =>

        val value = Seq(
          Some(HtmlFormat.escape(answer.desAddress.line1).toString),
          answer.desAddress.line2.map(HtmlFormat.escape),
          answer.desAddress.line3.map(HtmlFormat.escape),
          answer.desAddress.line4.map(HtmlFormat.escape),
          answer.desAddress.line5.map(HtmlFormat.escape),
          answer.desAddress.postCode.map(HtmlFormat.escape),
          Some(HtmlFormat.escape(answer.desAddress.countryCode).toString)
        ).flatten.mkString("<br/>")

        SummaryListRowViewModel(
          key = "clientBusinessAddress.checkYourAnswersLabel",
          value = ValueViewModel(HtmlContent(value))
        )
    }
  }

  def changeRegBusinessAddressRow(
                                   waypoints: Waypoints,
                                   answers: UserAnswers,
                                   sourcePage: CheckAnswersPage)(implicit messages: Messages): Option[SummaryListRow] = {
    answers.vatInfo.map {
      answer =>

        val value = Seq(
          Some(HtmlFormat.escape(answer.desAddress.line1).toString),
          answer.desAddress.line2.map(HtmlFormat.escape),
          answer.desAddress.line3.map(HtmlFormat.escape),
          answer.desAddress.line4.map(HtmlFormat.escape),
          answer.desAddress.line5.map(HtmlFormat.escape),
          answer.desAddress.postCode.map(HtmlFormat.escape),
        ).flatten.mkString("<br/>")

        SummaryListRowViewModel(
          key = "clientBusinessAddress.checkYourAnswersLabel",
          value = ValueViewModel(HtmlContent(value))
        )
    }
  }

  def changeRegVatBusinessNameRow(
                                   waypoints: Waypoints,
                                   answers: UserAnswers,
                                   sourcePage: CheckAnswersPage,
                                   basedInUk: Boolean)(implicit messages: Messages): Option[SummaryListRow] = {

    (answers.vatInfo, basedInUk) match {
      case (Some(vatCustomerInfo), basedInUk) if !basedInUk =>
        val clientName: String = vatCustomerInfo.organisationName.orElse(vatCustomerInfo.individualName).getOrElse(
          throw new IllegalStateException("Unable to retrieve a required client Name from the vat information")
        )
        val nonUkCountry: Country = answers.get(ClientCountryBasedPage).getOrElse(
          throw new IllegalStateException("Unable to retrieve country for non uk client")
        )

        Some(SummaryListRowViewModel(
            key = messages("clientBusinessName.checkYourAnswersLabel.withCountry", nonUkCountry.name),
            value = ValueViewModel(HtmlContent(HtmlFormat.escape(clientName).toString)),
            actions = Seq(
              ActionItemViewModel("site.change", sourcePage.changeLink(waypoints, sourcePage).url)
                .withVisuallyHiddenText(messages("clientBusinessName.change.hidden"))
            )
          ))

      case(_, _) =>
        None
    }
  }
}
