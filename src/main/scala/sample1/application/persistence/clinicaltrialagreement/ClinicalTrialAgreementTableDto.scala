package sample1.application.persistence.clinicaltrialagreement

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId}
import sample1.domain.entity.EntityVersion

case class ClinicalTrialAgreementTableDto(id: ClinicalTrialAgreementId,
                                          version: EntityVersion,
                                          note: String)

object ClinicalTrialAgreementTableDto {
  def apply(cta: ClinicalTrialAgreement): ClinicalTrialAgreementTableDto =
    new ClinicalTrialAgreementTableDto(cta.id, cta.version, cta.note)
}
