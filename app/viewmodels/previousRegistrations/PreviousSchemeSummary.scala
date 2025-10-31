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

package viewmodels.previousRegistrations

import models.previousRegistrations.SchemeDetailsWithOptionalVatNumber
import models.requests.DataRequest
import models.{Country, Index, PreviousScheme}
import pages.Waypoints
import pages.previousRegistrations.PreviousSchemePage
import play.api.i18n.Messages
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.*
import viewmodels.govuk.summarylist.*
import viewmodels.implicits.*

object PreviousSchemeSummary {

  def getSummaryLists(
                       previousSchemes: List[SchemeDetailsWithOptionalVatNumber],
                       countryIndex: Index,
                       country: Country,
                       existingSchemes: Seq[PreviousScheme],
                       waypoints: Waypoints
                     )(implicit request: DataRequest[_], messages: Messages): List[SummaryList] = {

    previousSchemes.zipWithIndex.flatMap { case (scheme, schemeIndex) =>
      val index = Index(schemeIndex)
      request.userAnswers.get(PreviousSchemePage(countryIndex, index)).map { (previousAnsweredScheme: PreviousScheme) =>
        val isExistingScheme = existingSchemes.contains(previousAnsweredScheme)

        val maybeOssRow = PreviousSchemeNumberSummary.row(request.userAnswers, countryIndex, index, scheme.previousScheme)
        val maybeIossRow = PreviousIossNumberSummary.row(request.userAnswers, countryIndex, index, scheme.previousScheme)

        val rows: Seq[SummaryListRow] = previousAnsweredScheme match {
          case PreviousScheme.OSSU | PreviousScheme.OSSNU =>
            Seq(maybeOssRow).flatten

          case PreviousScheme.IOSSWI =>
            Seq(maybeIossRow).flatten

          case PreviousScheme.IOSSWOI =>
            Seq(maybeIossRow).flatten
        }
        SummaryListViewModel(
          rows = rows
        ).withCard(
          card = Card(
            title = Some(CardTitle(content = HtmlContent(HtmlFormat.escape(messages(s"previousScheme.$previousAnsweredScheme"))))),
            actions = Some(Actions(
              items = if (!isExistingScheme) {
                Seq(
                  ActionItemViewModel(
                    "site.remove",
                    controllers.previousRegistrations.routes.DeletePreviousSchemeController.onPageLoad(waypoints, countryIndex, Index(schemeIndex)).url
                  ).withVisuallyHiddenText(messages("site.remove.hidden", country.name, HtmlFormat.escape(messages(s"previousScheme.$previousAnsweredScheme"))))
                )
              } else {
                Seq.empty
              }
            ))
          )
        )
      }
    }
  }
}
