package sample1.domain.command

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId}
import sample1.domain.entity.EntityRepo
import sample1.domain.user.UserId
import sample1.domain.{InvoiceError, StaleCtaError}

sealed trait CtaCreateCommand[F[_]] extends EntityCreateCommand[F, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement] {
  def create(): Either[InvoiceError, ClinicalTrialAgreement]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

sealed trait CtaUpdateCommand[F[_]] extends EntityUpdateCommand[F, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement] {
  def action(cta: ClinicalTrialAgreement): Either[InvoiceError, ClinicalTrialAgreement]

  override def staleF(id: ClinicalTrialAgreementId): InvoiceError = StaleCtaError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

final case class CtaRetrieveCommand[F[_]](userId: UserId, id: ClinicalTrialAgreementId) extends EntityRetrieveCommand[F, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

final case class CreateCtaCmd[F[_]](userId: UserId) extends CtaCreateCommand[F] {
  override def create(): Either[InvoiceError, ClinicalTrialAgreement] = Right(ClinicalTrialAgreement.create(this))
}
