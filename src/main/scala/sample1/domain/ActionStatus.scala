package sample1.domain

sealed trait ActionStatus

sealed trait NotAllowed extends ActionStatus

object ActionStatus {

  case object Allowed extends ActionStatus

  final case class AccessDenied() extends NotAllowed

  final case class NotAllowedInCurrentStatus() extends NotAllowed

  final case class NotAllowedForProcessType() extends NotAllowed

  final case class NotAllowedInCurrentState(reason: String) extends NotAllowed

  final case class NotEnoughPermissions(reason: String) extends NotAllowed

}
