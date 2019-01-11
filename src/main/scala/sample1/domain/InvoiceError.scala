package sample1.domain

import sample1.domain.cta.ClinicalTrialAgreementId
import sample1.domain.invoice.InvoiceId

sealed trait InvoiceError

final case class InvoiceNotFound(id: InvoiceId) extends InvoiceError

final case class CtaNotFound(id: ClinicalTrialAgreementId) extends InvoiceError

final case class StaleInvoiceError(id: InvoiceId) extends InvoiceError

final case class StaleCtaError(id: ClinicalTrialAgreementId) extends InvoiceError

final case class ActionBlocked() extends InvoiceError

final case class ActionNotAllowedForProcessType() extends InvoiceError

final case class ActionNotAllowedInCurrentStatus() extends InvoiceError

sealed trait ValidationError extends InvoiceError

final case class InvalidCurrencyCode(code: String) extends ValidationError

final case class CurrencyMismatch(first: Currency, second: Currency) extends ValidationError

object InvoiceError {
  def fromActionStatus(notAllowed: NotAllowed): InvoiceError = notAllowed match {
    case _: Blocked => ActionBlocked()
    case _: NotAllowedForProcessType => ActionNotAllowedForProcessType()
    case _: NotAllowedInCurrentStatus => ActionNotAllowedInCurrentStatus()
  }
}
