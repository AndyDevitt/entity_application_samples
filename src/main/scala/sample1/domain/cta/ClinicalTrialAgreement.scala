package sample1.domain.cta

import cats.data.NonEmptyList
import sample1.domain.command.CreateCtaCmd
import sample1.domain.entity.{EntityVersion, VersionedEntity}

case class ClinicalTrialAgreement(id: ClinicalTrialAgreementId,
                                  version: EntityVersion,
                                  amendments: NonEmptyList[CtaAmendment],
                                  note: String)
  extends VersionedEntity[ClinicalTrialAgreementId]

object ClinicalTrialAgreement {
  def create[F[_]](cmd: CreateCtaCmd[F]): ClinicalTrialAgreement =
    new ClinicalTrialAgreement(
      id = ClinicalTrialAgreementId(),
      version = EntityVersion(),
      amendments = NonEmptyList(CtaAmendment(cmd.effectiveDate, Set()), Nil),
      note = "")
}
