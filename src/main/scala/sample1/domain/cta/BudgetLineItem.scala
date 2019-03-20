package sample1.domain.cta

import sample1.domain.payee.PayeeId

sealed trait BudgetLineItem

case class SimpleBudgetLineItem(id: BudgetLineItemId,
                                context: ContextFilter,
                                condition: PaymentCondition,
                                payeeId: PayeeId
                               ) extends BudgetLineItem
