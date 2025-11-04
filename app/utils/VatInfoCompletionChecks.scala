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

import models.requests.DataRequest
import pages.*
import play.api.mvc.Results.Redirect
import play.api.mvc.{AnyContent, Result}

object VatInfoCompletionChecks extends CompletionChecks {

  def isBasedInUk()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage).isDefined
  }

  def incompleteBusinessBasedInUkRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    if (!isBasedInUk()) {
      Some(Redirect(controllers.routes.BusinessBasedInUKController.onPageLoad(waypoints)))
    } else {
      None
    }
  }

  def hasUkVatNumber()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        true
      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(true) =>
            request.userAnswers.get(ClientVatNumberPage).isDefined
          case Some(false) =>
            true
          case None =>
            false
        }
      case None =>
        false
    }

  }

  def incompleteHasVatNumberRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(true) if request.userAnswers.get(ClientHasVatNumberPage).isEmpty =>
        Some(Redirect(controllers.routes.ClientHasVatNumberController.onPageLoad(waypoints)))
      case _ =>
        None
    }
  }

  def ukVatNumberDefined()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        true
      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(true) =>
            request.userAnswers.get(ClientVatNumberPage).isDefined
          case Some(false) =>
            true
          case None =>
            false
        }
      case None => false
    }
  }

  def incompleteClientVatNumberRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(ClientHasVatNumberPage) match {
      case Some(true) if request.userAnswers.get(ClientVatNumberPage).isEmpty =>
        Some(Redirect(controllers.routes.ClientVatNumberController.onPageLoad(waypoints)))
      case _ =>
        None
    }
  }

  def hasUtrNumber()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        true
      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false) =>
            request.userAnswers.get(ClientHasUtrNumberPage).isDefined
          case Some(true) =>
            true
          case None =>
            false
        }
      case None =>
        false
    }


  }

  def incompleteHasUtrNumberRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        None
      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false) if request.userAnswers.get(ClientHasUtrNumberPage).isEmpty =>
            Some(Redirect(controllers.routes.ClientHasUtrNumberController.onPageLoad(waypoints)))
          case _ =>
            None
        }
      case None =>
        None
    }
  }

  def utrNumberDefined()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        true
      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false) =>
            request.userAnswers.get(ClientHasUtrNumberPage) match {
              case Some(true) =>
                request.userAnswers.get(ClientUtrNumberPage).isDefined
              case Some(false) =>
                true
              case None =>
                false
            }
          case Some(true) =>
            true
          case None =>
            false
        }
      case None =>
        false
    }
  }

  def incompleteClientUtrNumberRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(ClientHasUtrNumberPage) match {
      case Some(true) if request.userAnswers.get(ClientUtrNumberPage).isEmpty =>
        Some(Redirect(controllers.routes.ClientUtrNumberController.onPageLoad(waypoints)))
      case _ =>
        None
    }
  }

  def ninoNumberDefined()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        true
      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false) =>
            request.userAnswers.get(ClientHasUtrNumberPage) match {
              case Some(false) =>
                request.userAnswers.get(ClientsNinoNumberPage).isDefined
              case Some(true) =>
                true
              case None =>
                false
            }
          case Some(true) =>
            true
          case None =>
            false
        }
      case None =>
        false
    }
  }

  def incompleteClientsNinoNumberRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(ClientHasUtrNumberPage) match {
      case Some(false) if request.userAnswers.get(ClientsNinoNumberPage).isEmpty =>
        Some(Redirect(controllers.routes.ClientsNinoNumberController.onPageLoad(waypoints)))
      case _ =>
        None
    }
  }


  def clientBusinessAddressDefined()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        request.userAnswers.get(ClientBusinessAddressPage).isDefined

      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false) =>
            request.userAnswers.get(ClientBusinessAddressPage).isDefined
          case Some(true) =>
            true
          case None =>
            false
        }

      case None =>
        false
    }
  }

  def incompleteBusinessAddressRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        if (request.userAnswers.get(ClientBusinessAddressPage).isEmpty) {
          Some(Redirect(controllers.routes.ClientBusinessAddressController.onPageLoad(waypoints)))
        } else {
          None
        }

      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false) if request.userAnswers.get(ClientBusinessAddressPage).isEmpty =>
            Some(Redirect(controllers.routes.ClientBusinessAddressController.onPageLoad(waypoints)))
          case _ =>
            None
        }

      case None =>
        None
    }
  }

  def hasClientBusinessName()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) => request.userAnswers.get(ClientBusinessNamePage).isDefined

      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false) =>
            request.userAnswers.get(ClientBusinessNamePage).isDefined
          case Some(true) =>
            true
          case None =>
            false
        }

      case None =>
        false
    }

  }

  def incompleteClientBusinessNameRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        if (request.userAnswers.get(ClientBusinessNamePage).isEmpty) {
          Some(Redirect(controllers.routes.ClientBusinessNameController.onPageLoad(waypoints)))
        } else {
          None
        }
      case Some(true) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false) if request.userAnswers.get(ClientBusinessNamePage).isEmpty =>
            Some(Redirect(controllers.routes.ClientBusinessNameController.onPageLoad(waypoints)))
          case _ =>
            None
        }
      case None =>
        None
    }
  }

  def clientCountryBasedDefined()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        request.userAnswers.get(ClientCountryBasedPage).isDefined
      case Some(true) =>
        true
      case None =>
        false
    }
  }

  def incompleteClientCountryRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) if request.userAnswers.get(ClientCountryBasedPage).isEmpty =>
        Some(Redirect(controllers.routes.ClientCountryBasedController.onPageLoad(waypoints)))
      case _ =>
        None
    }
  }

  def clientTaxReferenceDefined()(implicit request: DataRequest[AnyContent]): Boolean = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(true) =>
            true
          case Some(false) =>
            request.userAnswers.get(ClientTaxReferencePage).isDefined
          case None =>
            false
        }
      case Some(true) =>
        true
      case None =>
        false
    }
  }

  def incompleteClientTaxReferenceRedirect(waypoints: Waypoints)(implicit request: DataRequest[AnyContent]): Option[Result] = {
    request.userAnswers.get(BusinessBasedInUKPage) match {
      case Some(false) =>
        request.userAnswers.get(ClientHasVatNumberPage) match {
          case Some(false)
            if request.userAnswers.get(ClientTaxReferencePage).isEmpty =>
            Some(Redirect(controllers.routes.ClientTaxReferenceController.onPageLoad(waypoints)))
          case _ =>
            None
        }
      case _ =>
        None
    }
  }
}
