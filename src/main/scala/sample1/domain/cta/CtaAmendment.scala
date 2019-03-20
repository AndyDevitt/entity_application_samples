package sample1.domain.cta

import sample1.domain.shared.DateTime

case class CtaAmendment(id: CtaAmendmentId,
                        effectiveDate: DateTime,
                        budgetLineItems: Set[BudgetLineItem])

object CtaAmendment {
  def apply(effectiveDate: DateTime,
            budgetLineItems: Set[BudgetLineItem]
           ): CtaAmendment =
    new CtaAmendment(CtaAmendmentId(), effectiveDate, budgetLineItems)
}
