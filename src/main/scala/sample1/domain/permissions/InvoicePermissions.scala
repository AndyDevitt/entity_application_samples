package sample1.domain.permissions

sealed trait InvoicePermissions

object InvoicePermissions {

  final case class Create() extends InvoicePermissions

  final case class Read() extends InvoicePermissions

  final case class Approve() extends InvoicePermissions

  final case class Reject() extends InvoicePermissions

  final case class AddCost() extends InvoicePermissions

  final case class ApproveWithLimit(limit: Int) extends InvoicePermissions

}

final case class InvoiceUserPermissions(permissions: Set[InvoicePermissions]) extends EntityPermissions[InvoicePermissions] {
  def hasReadPermission: Boolean = permissions.contains(InvoicePermissions.Read())

  def hasCreatePermission: Boolean = permissions.contains(InvoicePermissions.Create())
}
