package sample1.domain.cta

sealed trait PaymentCondition

sealed trait CustomCondition extends PaymentCondition

sealed trait OtbCondition extends PaymentCondition

sealed trait VisitCondition extends PaymentCondition

sealed trait ProcedureCondition extends PaymentCondition

object PaymentCondition {

  final case object VisitDateEntered extends OtbCondition with VisitCondition

  final case object AllDateEntered extends OtbCondition with ProcedureCondition

}
