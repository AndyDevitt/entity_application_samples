package sample1.domain.invoice

import sample1.domain.entity.EntityId

import scala.util.Random

final case class InvoiceId(value: Int) extends EntityId

object InvoiceId {
  def apply(): InvoiceId = new InvoiceId(Random.nextInt().abs)
}
