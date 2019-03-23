package sample1.application.persistence.clinicaltrialagreement

import sample1.domain.cta.ClinicalTrialAgreement

case class ClinicalTrialAgreementDto(state: ClinicalTrialAgreementTableDto,
                                     amendments: List[ClinicalTrialAgreementAmendmentDto],
                                     budgetLineItems: List[BudgetLineItemDto])

object ClinicalTrialAgreementDto {
  def apply(cta: ClinicalTrialAgreement): ClinicalTrialAgreementDto =
    new ClinicalTrialAgreementDto(
      state = ClinicalTrialAgreementTableDto(cta),
      amendments = cta.amendments.toList.map(ClinicalTrialAgreementAmendmentDto(_, cta)),
      budgetLineItems = cta.amendments.toList.flatMap(a => a.budgetLineItems.map(BudgetLineItemDto(_, cta, a))))
}
