package sample1.infrastructure

import cats.Monad
import cats.effect.IO
import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId, CtaRepo}
import sample1.domain.entity.Versioned
import sample1.domain.{CtaNotFound, InvoiceError, StaleCtaError}

class ProductionCtaRepo(implicit versioned: Versioned[ClinicalTrialAgreement]) extends CtaRepo[IO] {
  private val inMemoryRepo: InMemoryRepo[IO, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] =
    new InMemoryRepo[IO, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] {
      override def notFoundErrorF: ClinicalTrialAgreementId => InvoiceError = id => CtaNotFound(id)

      override def staleErrorF: ClinicalTrialAgreementId => InvoiceError = id => StaleCtaError(id)
    }

  override def save(entity: ClinicalTrialAgreement)(implicit monad: Monad[IO]): IO[Either[InvoiceError, ClinicalTrialAgreement]] =
    inMemoryRepo.saveEntity(entity)

  override def retrieve(id: ClinicalTrialAgreementId)(implicit monad: Monad[IO]): IO[Either[InvoiceError, ClinicalTrialAgreement]] =
    inMemoryRepo.retrieveEntity(id)
}
