package sample1.domain.cta

import sample1.domain.entity.EntityId
import sample1.domain.shared.Uuid

case class CtaAmendmentId(value: Uuid) extends EntityId

object CtaAmendmentId {
  def apply(): CtaAmendmentId = new CtaAmendmentId(Uuid())
}
