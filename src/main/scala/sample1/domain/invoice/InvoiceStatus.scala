package sample1.domain.invoice

sealed trait InvoiceStatus

object InvoiceStatus {

  case object Draft extends InvoiceStatus

  case object Assigned extends InvoiceStatus

  case object ReadyToSend extends InvoiceStatus

  case object Approved extends InvoiceStatus

  case object NotApproved extends InvoiceStatus

  case object Sent extends InvoiceStatus

}
