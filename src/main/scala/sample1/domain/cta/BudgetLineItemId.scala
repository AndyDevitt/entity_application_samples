package sample1.domain.cta

import sample1.domain.entity.EntityId
import sample1.domain.shared.Uuid

case class BudgetLineItemId(value: Uuid) extends EntityId

object BudgetLineItemId {
  def apply(): BudgetLineItemId = new BudgetLineItemId(Uuid())
}
