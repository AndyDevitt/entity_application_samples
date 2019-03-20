package sample1.domain.payee

import sample1.domain.shared.Uuid

case class PayeeId(value: Uuid)

object PayeeId {
  def apply(): PayeeId = new PayeeId(Uuid())
}
