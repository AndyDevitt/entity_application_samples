package sample1.application.persistence.clinicaltrialagreement

import sample1.domain.cta.ClinicalTrialAgreement
import sample1.domain.entity.{EntityId, PersistenceRepo, VersionedEntity}
import sample1.domain.errors.CtaError

trait CtaPersistenceRepo[F[_], PersistenceIdType <: EntityId, PersistenceType <: VersionedEntity[PersistenceIdType]]
  extends PersistenceRepo[F, PersistenceIdType, PersistenceType, CtaError] {
  def find(): F[Either[CtaError, Seq[ClinicalTrialAgreement]]]
}
