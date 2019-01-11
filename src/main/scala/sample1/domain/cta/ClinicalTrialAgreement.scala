package sample1.domain.cta

import sample1.domain.command.CreateCtaCmdG
import sample1.domain.entity.{EntityVersion, VersionedEntity}

import scala.util.Random

case class ClinicalTrialAgreement(id: ClinicalTrialAgreementId, version: EntityVersion, note: String) extends VersionedEntity[ClinicalTrialAgreement, ClinicalTrialAgreementId] {

}

object ClinicalTrialAgreement {
  def create[F[_]](cmd: CreateCtaCmdG[F]): ClinicalTrialAgreement = new ClinicalTrialAgreement(id = ClinicalTrialAgreementId(Random.nextInt().abs), EntityVersion(), "")
}
