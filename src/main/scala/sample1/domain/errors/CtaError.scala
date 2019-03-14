package sample1.domain

import sample1.domain.cta.ClinicalTrialAgreementId

sealed trait CtaError

object CtaError {

  final case class CtaNotFound(id: ClinicalTrialAgreementId) extends CtaError

  final case class StaleCtaError(id: ClinicalTrialAgreementId) extends CtaError

}
