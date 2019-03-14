package sample1.domain.permissions

sealed trait InvoicePermissions

object InvoicePermissions {

  final case class Create()

  final case class Read()

  final case class Approve()

  final case class Reject()

  final case class AddCost()

  final case class ApproveWithLimit(limit: Int)

}
