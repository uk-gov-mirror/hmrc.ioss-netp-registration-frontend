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

package controllers

import controllers.actions.*
import logging.Logging
import models.UserAnswers
import models.domain.VatCustomerInfo
import pages.*
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.SummaryList
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.CompletionChecks
import utils.FutureSyntax.FutureOps
import viewmodels.CheckVatDetailsViewModel
import viewmodels.checkAnswers.*
import viewmodels.govuk.all.SummaryListViewModel
import views.html.CheckVatDetailsView

import javax.inject.Inject

class CheckVatDetailsController @Inject()(
                                           override val messagesApi: MessagesApi,
                                           cc: AuthenticatedControllerComponents,
                                           view: CheckVatDetailsView
                                         )() extends FrontendBaseController with I18nSupport with GetClientCompanyName with Logging with CompletionChecks {

  protected val controllerComponents: MessagesControllerComponents = cc

  def onPageLoad(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetData().async {
    implicit request =>

      val isBasedInUk = request.userAnswers.get(BusinessBasedInUKPage).getOrElse(false)
      val hasVatNumber = request.userAnswers.get(ClientHasVatNumberPage).getOrElse(false)
      val summaryList = buildSummaryList(waypoints, request.userAnswers, CheckVatDetailsPage())
      val ukVatNumber = request.userAnswers.get(ClientVatNumberPage).getOrElse("")

      getClientCompanyName(waypoints) { clientCompanyName =>

        if (isBasedInUk && hasVatNumber) {
          request.userAnswers.vatInfo match {
            case Some(vatCustomerInfo) =>

              val viewModel = CheckVatDetailsViewModel(ukVatNumber, vatCustomerInfo)

              val isValid: Boolean = validate()

              Ok(view(waypoints, Some(viewModel), summaryList, clientCompanyName, isBasedInUk, hasVatNumber, isValid)).toFuture

            case None =>
              Redirect(UkVatNumberNotFoundPage.route(waypoints).url).toFuture
          }
        } else {
          val isValid: Boolean = validate()

          Ok(view(waypoints, None, summaryList, clientCompanyName, isBasedInUk, hasVatNumber, isValid)).toFuture
        }
      }
  }

  def onSubmit(waypoints: Waypoints, incompletePrompt: Boolean): Action[AnyContent] = cc.identifyAndGetData().async {
    implicit request =>

      getFirstValidationErrorRedirect(waypoints) match {
        case Some(errorRedirect) =>
          if (incompletePrompt) {
            errorRedirect.toFuture
          } else {
            Redirect(CheckVatDetailsPage().route(waypoints).url).toFuture
          }
        case None =>
          Redirect(CheckVatDetailsPage().navigate(waypoints, request.userAnswers, request.userAnswers).route).toFuture
      }
  }

  private def buildSummaryList(
                                waypoints: Waypoints,
                                userAnswers: UserAnswers,
                                sourcePage: CheckAnswersPage)
                              (implicit messages: Messages): SummaryList = {

    val hasUkVatNumber = userAnswers.get(ClientHasVatNumberPage).contains(true)
    val isUKBased = userAnswers.get(BusinessBasedInUKPage).contains(true)

    val rows = Seq(
      BusinessBasedInUKSummary.row(waypoints, userAnswers, sourcePage),
      ClientHasVatNumberSummary.row(waypoints, userAnswers, sourcePage),
      ClientVatNumberSummary.row(waypoints, userAnswers, sourcePage),
      ClientCountryBasedSummary.row(waypoints, userAnswers, sourcePage),
      ClientTaxReferenceSummary.row(waypoints, userAnswers, sourcePage),
      ClientBusinessNameSummary.row(waypoints, userAnswers, sourcePage),
      if (isUKBased && !hasUkVatNumber) ClientHasUtrNumberSummary.row(waypoints, userAnswers, sourcePage) else None,
      ClientUtrNumberSummary.row(waypoints, userAnswers, sourcePage),
      ClientsNinoNumberSummary.row(waypoints, userAnswers, sourcePage),
      ClientBusinessAddressSummary.row(waypoints, userAnswers, sourcePage)
    ).flatten

    SummaryListViewModel(rows = rows)
  }

}
