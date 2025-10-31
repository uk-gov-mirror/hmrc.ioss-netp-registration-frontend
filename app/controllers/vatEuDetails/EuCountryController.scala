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

package controllers.vatEuDetails

import controllers.actions.*
import forms.vatEuDetails.EuCountryFormProvider
import models.{Country, Index, UserAnswers}
import pages.Waypoints
import pages.vatEuDetails.*
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.euDetails.AllEuDetailsQuery
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.vatEuDetails.EuCountryView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

class EuCountryController @Inject()(
                                       override val messagesApi: MessagesApi,
                                       cc: AuthenticatedControllerComponents,
                                       formProvider: EuCountryFormProvider,
                                       view: EuCountryView
                                     )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport {

  protected val controllerComponents: MessagesControllerComponents = cc
  
  def onPageLoad(waypoints: Waypoints, countryIndex: Index): Action[AnyContent] = cc.identifyAndGetData() {
    implicit request =>

      val form: Form[Country] = formProvider(countryIndex, request.userAnswers.get(AllEuDetailsQuery)
        .getOrElse(Seq.empty).map(_.euCountry))

      val preparedForm = request.userAnswers.get(EuCountryPage(countryIndex)) match {
        case None => form
        case Some(value) => form.fill(value)
      }

      Ok(view(preparedForm, waypoints, countryIndex))
  }

  def onSubmit(waypoints: Waypoints, countryIndex: Index): Action[AnyContent] = cc.identifyAndGetData().async {
    implicit request =>
 
      val form: Form[Country] = formProvider(countryIndex, request.userAnswers.get(AllEuDetailsQuery)
        .getOrElse(Seq.empty).map(_.euCountry))

      form.bindFromRequest().fold(
        formWithErrors =>
          Future.successful(BadRequest(view(formWithErrors, waypoints, countryIndex))),

        value =>
          val existingCountry = request.userAnswers.get(EuCountryPage(countryIndex))

          val cleanedAnswersTry: Try[UserAnswers] =
            (existingCountry, Some(value)) match {
              case (Some(oldCountry), Some(newCountry)) if oldCountry != newCountry =>
                cleanUp(countryIndex, request.userAnswers)
              case _ =>
                Success(request.userAnswers)
            }

          for {
            cleanedAnswers <- Future.fromTry(cleanedAnswersTry)
            updatedAnswers <- Future.fromTry(cleanedAnswers.set(EuCountryPage(countryIndex), value))
            _              <- cc.sessionRepository.set(updatedAnswers)
          } yield Redirect(EuCountryPage(countryIndex).navigate(waypoints, request.userAnswers, updatedAnswers).route)
      )
  }

  private def cleanUp(countryIndex: Index, answers: UserAnswers): Try[UserAnswers] = {
    for {
      remove1 <- answers.remove(TradingNameAndBusinessAddressPage(countryIndex))
      remove2 <- remove1.remove(RegistrationTypePage(countryIndex))
      remove3 <- remove2.remove(EuVatNumberPage(countryIndex))
      cleaned <- remove3.remove(EuTaxReferencePage(countryIndex))
    } yield cleaned
  }
}
