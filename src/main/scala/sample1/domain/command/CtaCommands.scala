package sample1.domain.command

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId}
import sample1.domain.entity.EntityRepo
import sample1.domain.user.UserId
import sample1.domain.{InvoiceError, StaleCtaError}

sealed trait CtaPermissions

sealed trait CtaCreateCommand[F[_], H[_]] extends EntityCreateCommand[F, H, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaPermissions] {
  def create(): Either[InvoiceError, ClinicalTrialAgreement]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

sealed trait CtaUpdateCommand[F[_], H[_], CmdType] extends EntityUpdateCommand[F, H, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaPermissions] {
  def action(cta: ClinicalTrialAgreement): Either[InvoiceError, ClinicalTrialAgreement]

  override def staleF(id: ClinicalTrialAgreementId): InvoiceError = StaleCtaError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

final case class CtaRetrieveCommand[F[_], H[_]](userId: UserId, id: ClinicalTrialAgreementId) extends EntityRetrieveCommand[F, H, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaPermissions] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

final case class CreateCtaCmd[F[_], H[_]](userId: UserId) extends CtaCreateCommand[F, H] {
  override def create(): Either[InvoiceError, ClinicalTrialAgreement] = Right(ClinicalTrialAgreement.create(this))
}
