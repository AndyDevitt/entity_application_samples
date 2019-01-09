package sample1.domain

sealed trait ActionStatus

case object Allowed extends ActionStatus

sealed trait NotAllowed extends ActionStatus

final case class Blocked() extends NotAllowed

final case class NotAllowedInCurrentStatus() extends NotAllowed

final case class NotAllowedForProcessType() extends NotAllowed
