package sample1.domain.invoice

import scala.util.Random

trait EntityId

final case class InvoiceId(value: Int) extends EntityId

object InvoiceId {
  def apply(): InvoiceId = new InvoiceId(Random.nextInt().abs)
}
