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

import models.Index
import models.requests.DataRequest
import pages.*
import pages.tradingNames.{AddTradingNamePage, HasTradingNamePage, TradingNamePage}
import play.api.mvc.Results.Redirect
import play.api.mvc.{AnyContent, Result}
import queries.AllWebsites
import queries.tradingNames.AllTradingNamesQuery
import utils.EuDetailsCompletionChecks.*
import utils.PreviousRegistrationsCompletionChecks.*
import utils.VatInfoCompletionChecks.*

import scala.concurrent.Future

trait CompletionChecks {

  protected def withCompleteDataModel[A](index: Index, data: Index => Option[A], onFailure: Option[A] => Result)
                                        (onSuccess: => Result): Result = {
    val incomplete = data(index)
    if (incomplete.isEmpty) {
      onSuccess
    } else {
      onFailure(incomplete)
    }
  }

  protected def withCompleteDataAsync[A](data: () => Seq[A], onFailure: Seq[A] => Future[Result])
                                        (onSuccess: => Future[Result]): Future[Result] = {
    val incomplete = data()
    if (incomplete.isEmpty) {
      onSuccess
    } else {
      onFailure(incomplete)
    }
  }

  def validate()(implicit request: DataRequest[AnyContent]): Boolean = {
    isBasedInUk() &&
      hasUkVatNumber() &&
      ukVatNumberDefined() &&
      clientCountryBasedDefined() &&
      hasClientBusinessName() &&
      hasUtrNumber() &&
      utrNumberDefined() &&
      ninoNumberDefined() &&
      clientTaxReferenceDefined() &&
      clientBusinessAddressDefined() &&
      isTradingNamesValid() &&
      !hasUnfilledTradingNamePageAttempt() &&
      getAllIncompleteRegistrationDetails().isEmpty &&
      isPreviouslyRegisteredDefined() &&
      isEuDetailsDefined() &&
      getAllIncompleteEuDetails().isEmpty &&
      hasWebsiteValid()
  }

  def getFirstValidationErrorRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    (incompleteBusinessBasedInUkRedirect(waypoints) ++
      incompleteHasVatNumberRedirect(waypoints) ++
      incompleteClientVatNumberRedirect(waypoints) ++
      incompleteClientCountryRedirect(waypoints) ++
      incompleteClientBusinessNameRedirect(waypoints) ++
      incompleteHasUtrNumberRedirect(waypoints) ++
      incompleteClientUtrNumberRedirect(waypoints) ++
      incompleteClientsNinoNumberRedirect(waypoints) ++
      incompleteClientTaxReferenceRedirect(waypoints) ++
      incompleteBusinessAddressRedirect(waypoints) ++
      incompleteHasTradingNameRedirect(waypoints) ++
      incompleteTradingNameRedirect(waypoints) ++
      incompleteAdditionalTradingNameRedirect(waypoints) ++
      emptyPreviousRegistrationRedirect(waypoints) ++
      incompletePreviousRegistrationRedirect(waypoints) ++
      emptyEuDetailsRedirect(waypoints) ++
      incompleteEuDetailsRedirect(waypoints) ++
      incompleteWebsiteUrlsRedirect(waypoints)
      ).headOption
  }

  private def hasWebsiteValid()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(AllWebsites).getOrElse(List.empty).nonEmpty
  }

  private def incompleteWebsiteUrlsRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = if (!hasWebsiteValid()) {
    Some(Redirect(controllers.website.routes.WebsiteController.onPageLoad(waypoints, Index(0))))
  } else {
    None
  }

  private def isTradingNamesValid()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(HasTradingNamePage).exists {
      case true => request.userAnswers.get(AllTradingNamesQuery).getOrElse(List.empty).nonEmpty
      case false => request.userAnswers.get(AllTradingNamesQuery).getOrElse(List.empty).isEmpty
    }
  }

  private def incompleteHasTradingNameRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    if (request.userAnswers.get(HasTradingNamePage).isEmpty) {
      Some(Redirect(HasTradingNamePage.route(waypoints)))
    } else {
      None
    }
  }


  private def incompleteTradingNameRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(HasTradingNamePage) match {
      case Some(true) =>
        val tradingNames = request.userAnswers.get(AllTradingNamesQuery).getOrElse(Nil)
        if (tradingNames.isEmpty) {
          Some(Redirect(controllers.tradingNames.routes.TradingNameController.onPageLoad(waypoints, Index(0))))
        } else {
          None
        }
      case _ => None
    }
  }

  private def hasUnfilledTradingNamePageAttempt()(implicit request: DataRequest[AnyContent]): Boolean = {
    val allAnswers = request.userAnswers
    val tradingNames = allAnswers.get(AllTradingNamesQuery).getOrElse(Nil)

    val declaredCount = tradingNames.length
    val nextIndex = Index(declaredCount)

    val addAnother = allAnswers.get(AddTradingNamePage(Some(Index(declaredCount - 1)))).contains(true)

    addAnother && allAnswers.get(TradingNamePage(nextIndex)).isEmpty
  }

  private def incompleteAdditionalTradingNameRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    val tradingNames = request.userAnswers.get(AllTradingNamesQuery).getOrElse(Nil)
    val lastCompletedIndex = Index(tradingNames.length - 1)
    val nextIndex = Index(tradingNames.length)

    val addAnother = request.userAnswers.get(AddTradingNamePage(Some(lastCompletedIndex))).contains(true)

    if (addAnother && request.userAnswers.get(TradingNamePage(nextIndex)).isEmpty) {
      Some(Redirect(controllers.tradingNames.routes.TradingNameController.onPageLoad(waypoints, nextIndex)))
    } else {
      None
    }
  }

}
