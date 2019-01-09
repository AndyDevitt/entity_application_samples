package sample1.domain.invoice

sealed trait InvoiceStatus

case object Approved extends InvoiceStatus

case object NotApproved extends InvoiceStatus
