package sample1.domain.permissions

sealed trait CtaPermissions

object CtaPermissions {

  final case class Create()

  final case class Read()

  final case class Approve()

  final case class Reject()

  final case class AddCost()

  final case class ApproveWithLimit(limit: Int)

}

final case class CtaUserPermissions(permissions: Set[CtaPermissions])