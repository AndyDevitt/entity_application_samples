package sample1.domain.cta

sealed trait CtaAction

object CtaAction {

  final case class Approve() extends CtaAction

}
