package sample1.domain.permissions

sealed trait InvoicePermissions

object InvoicePermissions {

  final case object Create extends InvoicePermissions

  final case object Read extends InvoicePermissions

  final case object Approve extends InvoicePermissions

  final case object Reject extends InvoicePermissions

  final case object AddCost extends InvoicePermissions

  final case class ApproveWithLimit(limit: Int) extends InvoicePermissions

}

final case class InvoiceUserPermissions(permissions: Set[InvoicePermissions]) extends EntityPermissions[InvoicePermissions] {
  def hasReadPermission: Boolean = permissions.contains(InvoicePermissions.Read)

  def hasCreatePermission: Boolean = permissions.contains(InvoicePermissions.Create)
}
