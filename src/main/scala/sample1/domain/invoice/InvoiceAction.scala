package sample1.domain.invoice

sealed trait InvoiceAction

object InvoiceAction {

  case object Approve extends InvoiceAction

  case object AssignToPayee extends InvoiceAction

  case object MarkAsReadyToSend extends InvoiceAction

  case object Withdraw extends InvoiceAction

  case object UpdateRfi extends InvoiceAction

  case object AddCost extends InvoiceAction

}
