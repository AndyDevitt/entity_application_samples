package sample1.domain.permissions

sealed trait CtaPermissions

object CtaPermissions {

  final case class Create() extends CtaPermissions

  final case class Read() extends CtaPermissions

  final case class Approve() extends CtaPermissions

  final case class Reject() extends CtaPermissions

  final case class AddCost() extends CtaPermissions

  final case class ApproveWithLimit(limit: Int) extends CtaPermissions

}

final case class CtaUserPermissions(permissions: Set[CtaPermissions]) {
  def hasReadPermission: Boolean = permissions.contains(CtaPermissions.Read())
}
