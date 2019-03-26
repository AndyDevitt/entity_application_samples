package sample1.infrastructure

import cats.{Id, Monad}
import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId, CtaRepo}
import sample1.domain.entity.{EntityRepoCodec, EntityRepoImpl, PersistenceRepo, Versioned}
import sample1.domain.errors.CtaError

class TestCtaRepo(override val persistenceRepo: PersistenceRepo[Id, CtaError, ClinicalTrialAgreement, ClinicalTrialAgreementId],
                  override val codec: EntityRepoCodec[ClinicalTrialAgreement, ClinicalTrialAgreement, CtaError])
                 (implicit versioned: Versioned[ClinicalTrialAgreement])
  extends CtaRepo[Id] with EntityRepoImpl[Id, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaError, ClinicalTrialAgreement, ClinicalTrialAgreementId] {

  override def save(entity: ClinicalTrialAgreement)(implicit monad: Monad[Id]): Id[Either[CtaError, ClinicalTrialAgreement]] =
    saveEntity(entity)

  override def retrieve(id: ClinicalTrialAgreementId)(implicit monad: Monad[Id]): Id[Either[CtaError, ClinicalTrialAgreement]] =
    retrieveEntity(id)
}
