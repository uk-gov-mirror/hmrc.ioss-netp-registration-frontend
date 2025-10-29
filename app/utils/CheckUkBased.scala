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

import config.Constants.ukCountryCodeAreaPrefix
import models.domain.VatCustomerInfo
import models.etmp.EtmpOtherAddress

object CheckUkBased {

  def isUkBasedNetp(vatCustomerInfo: Option[VatCustomerInfo], otherAddress: Option[EtmpOtherAddress]): Boolean = {

    (vatCustomerInfo, otherAddress) match {
      case (_, Some(address)) =>
        address.issuedBy.startsWith(ukCountryCodeAreaPrefix)
      case (Some(vatInfo), None) =>
        vatInfo.desAddress.countryCode.startsWith(ukCountryCodeAreaPrefix)
      case (None, None) =>
        throw new IllegalStateException(s"Unable to identify if client is based in the UK. " +
          s"Client requires either Vat Customer Info or an Etmp Other Address from ETMP for amend journey.")

    }
  }
}
