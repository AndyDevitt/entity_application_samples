package sample1.infrastructure

import cats.{Id, Monad}
import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId, CtaRepo}
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.errors.CtaError

class TestCtaRepo()(implicit versioned: Versioned[ClinicalTrialAgreement], codec: EntityRepoCodec[ClinicalTrialAgreement, ClinicalTrialAgreement, CtaError]) extends CtaRepo[Id] {
  self =>
  private val inMemoryRepo: InMemoryRepo[Id, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaError] =
    new InMemoryRepo[Id, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaError] {
      override def notFoundErrorF: ClinicalTrialAgreementId => CtaError = id => CtaError.CtaNotFound(id)

      override def staleErrorF: ClinicalTrialAgreementId => CtaError = id => CtaError.StaleCtaError(id)

      override def codec: EntityRepoCodec[ClinicalTrialAgreement, ClinicalTrialAgreement, CtaError] = self.codec
    }

  override def save(entity: ClinicalTrialAgreement)(implicit monad: Monad[Id]): Id[Either[CtaError, ClinicalTrialAgreement]] =
    inMemoryRepo.saveEntity(entity)

  override def retrieve(id: ClinicalTrialAgreementId)(implicit monad: Monad[Id]): Id[Either[CtaError, ClinicalTrialAgreement]] =
    inMemoryRepo.retrieveEntity(id)
}
