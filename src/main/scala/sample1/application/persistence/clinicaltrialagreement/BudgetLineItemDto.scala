package sample1.application.persistence.clinicaltrialagreement

import sample1.domain.cta._
import sample1.domain.entity.EntityVersion
import sample1.domain.shared.Uuid

case class BudgetLineItemDto(id: BudgetLineItemId,
                             context: ContextFilter,
                             condition: String,
                             payeeId: Uuid,
                             ctaId: ClinicalTrialAgreementId,
                             ctaVersion: EntityVersion,
                             amendmentId: CtaAmendmentId)

object BudgetLineItemDto {
  def apply(bli: BudgetLineItem, cta: ClinicalTrialAgreement, amendment: CtaAmendment): BudgetLineItemDto = bli match {
    case SimpleBudgetLineItem(id, context, condition, payeeId) =>
      new BudgetLineItemDto(id, context, condition.toString, payeeId.value, cta.id, cta.version, amendment.id)
  }
}
