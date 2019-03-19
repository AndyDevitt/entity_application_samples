package sample1.domain.permissions

sealed trait CtaPermissions

object CtaPermissions {

  final case object Create extends CtaPermissions

  final case object Read extends CtaPermissions

  final case object Approve extends CtaPermissions

  final case object Reject extends CtaPermissions

  final case object AddCost extends CtaPermissions

  final case class ApproveWithLimit(limit: Int) extends CtaPermissions

}

final case class CtaUserPermissions(permissions: Set[CtaPermissions]) extends EntityPermissions[CtaPermissions] {
  def hasReadPermission: Boolean = permissions.contains(CtaPermissions.Read)
}
