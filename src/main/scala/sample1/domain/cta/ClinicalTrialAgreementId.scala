package sample1.domain.cta

import sample1.domain.entity.EntityId
import sample1.domain.shared.Uuid

case class ClinicalTrialAgreementId(value: Uuid) extends EntityId

object ClinicalTrialAgreementId {
  def apply(): ClinicalTrialAgreementId = new ClinicalTrialAgreementId(Uuid())
}
