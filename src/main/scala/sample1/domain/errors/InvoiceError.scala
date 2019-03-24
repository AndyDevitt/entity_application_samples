package sample1.domain.errors

import sample1.domain._
import sample1.domain.invoice.InvoiceId

sealed trait InvoiceError {
  def msg: String = ""
}

sealed trait PermissionsError extends InvoiceError

sealed trait ValidationError extends InvoiceError

object InvoiceError {

  final case class InvoiceNotFound(id: InvoiceId) extends InvoiceError

  final case class StaleInvoiceError(id: InvoiceId) extends InvoiceError

  final case class ActionBlocked() extends InvoiceError

  final case class ActionNotAllowedForProcessType() extends InvoiceError

  final case class ActionNotAllowedInCurrentStatus() extends InvoiceError

  final case class ActionNotAllowedInCurrentState(reason: String) extends InvoiceError

  final case class CannotApproveWithoutCosts() extends InvoiceError

  final case class InsufficientPermissions() extends PermissionsError


  final case class InvalidCurrencyCode(code: String) extends ValidationError

  final case class CurrencyMismatch(first: Currency, second: Currency) extends ValidationError

  def fromActionStatus(notAllowed: NotAllowed): InvoiceError = notAllowed match {
    case Blocked() => ActionBlocked()
    case NotAllowedForProcessType() => ActionNotAllowedForProcessType()
    case NotAllowedInCurrentStatus() => ActionNotAllowedInCurrentStatus()
    case NotAllowedInCurrentState(reason) => ActionNotAllowedInCurrentState(reason)
    case NotEnoughPermissions(reason) => InsufficientPermissions()
  }
}
