package sample1.application.persistence.clinicaltrialagreement

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId}
import sample1.domain.entity.EntityVersion

case class ClinicalTrialAgreementDto(id: ClinicalTrialAgreementId,
                                     version: EntityVersion,
                                     note: String,
                                     amendments: List[ClinicalTrialAgreementAmendmentDto])

object ClinicalTrialAgreementDto {
  def apply(cta: ClinicalTrialAgreement): ClinicalTrialAgreementDto =
    new ClinicalTrialAgreementDto(
      id = cta.id,
      version = cta.version,
      note = cta.note,
      amendments = cta.amendments.map(ClinicalTrialAgreementAmendmentDto(_, cta)).toList)
}
