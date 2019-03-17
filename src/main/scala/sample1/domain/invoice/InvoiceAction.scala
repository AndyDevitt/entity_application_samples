package sample1.domain.invoice

sealed trait InvoiceAction

object InvoiceAction {

  case object Approve extends InvoiceAction

  case object UpdateRfi extends InvoiceAction

}
