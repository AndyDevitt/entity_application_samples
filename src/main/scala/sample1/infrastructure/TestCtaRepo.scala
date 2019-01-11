package sample1.infrastructure

import cats.{Id, Monad}
import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId, CtaRepo}
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.{CtaNotFound, InvoiceError, StaleCtaError}

class TestCtaRepo()(implicit versioned: Versioned[ClinicalTrialAgreement], codec: EntityRepoCodec[ClinicalTrialAgreement, ClinicalTrialAgreement, InvoiceError]) extends CtaRepo[Id] {
  private val inMemoryRepo: InMemoryRepo[Id, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] =
    new InMemoryRepo[Id, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] {
      override def notFoundErrorF: ClinicalTrialAgreementId => InvoiceError = id => CtaNotFound(id)

      override def staleErrorF: ClinicalTrialAgreementId => InvoiceError = id => StaleCtaError(id)
    }

  override def save(entity: ClinicalTrialAgreement)(implicit monad: Monad[Id]): Id[Either[InvoiceError, ClinicalTrialAgreement]] =
    inMemoryRepo.saveEntity(entity)

  override def retrieve(id: ClinicalTrialAgreementId)(implicit monad: Monad[Id]): Id[Either[InvoiceError, ClinicalTrialAgreement]] =
    inMemoryRepo.retrieveEntity(id)
}
