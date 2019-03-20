package sample1.application.persistence.clinicaltrialagreement

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId, CtaAmendment, CtaAmendmentId}
import sample1.domain.entity.EntityVersion

case class ClinicalTrialAgreementAmendmentDto(id: CtaAmendmentId,
                                              budgetLineItems: Set[BudgetLineItemDto],
                                              ctaId: ClinicalTrialAgreementId,
                                              ctaVersion: EntityVersion)

object ClinicalTrialAgreementAmendmentDto {
  def apply(ctaAmendment: CtaAmendment, cta: ClinicalTrialAgreement): ClinicalTrialAgreementAmendmentDto =
    new ClinicalTrialAgreementAmendmentDto(
      id = ctaAmendment.id,
      budgetLineItems = ctaAmendment.budgetLineItems.map(BudgetLineItemDto(_, cta, ctaAmendment)),
      ctaId = cta.id,
      ctaVersion = cta.version)
}
