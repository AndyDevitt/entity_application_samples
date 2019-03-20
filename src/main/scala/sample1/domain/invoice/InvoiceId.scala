package sample1.domain.invoice

import sample1.domain.entity.EntityId
import sample1.domain.shared.Uuid

final case class InvoiceId(value: Uuid) extends EntityId

object InvoiceId {
  def apply(): InvoiceId = new InvoiceId(Uuid())
}
