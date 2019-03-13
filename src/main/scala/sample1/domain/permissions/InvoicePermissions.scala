package sample1.domain.permissions

sealed trait InvoicePermissions

object InvoicePermissions {

  final case object Create

  final case object Read

  final case object Approve

  final case object Reject

  final case object AddCost

  final case class ApproveWithLimit(limit: Int)

}
