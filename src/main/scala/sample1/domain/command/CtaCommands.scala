package sample1.domain.command

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId}
import sample1.domain.entity.EntityRepo
import sample1.domain.permissions.{CtaBasicPermissionRetriever, CtaUserPermissions}
import sample1.domain.user.UserId
import sample1.domain.{InvoiceError, StaleCtaError}

sealed trait CtaPermissions

sealed trait CtaCreateCommand[F[_]] extends EntityCreateCommand[F,  DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaUserPermissions] {
  def create(): Either[InvoiceError, ClinicalTrialAgreement]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

sealed trait CtaUpdateCommand[F[_], H[_], CmdType] extends EntityUpdateCommand[F,  DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaUserPermissions] {
  def action(cta: ClinicalTrialAgreement): Either[InvoiceError, ClinicalTrialAgreement]

  override def staleF(id: ClinicalTrialAgreementId): InvoiceError = StaleCtaError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

final case class CtaRetrieveCommand[F[_]](userId: UserId, id: ClinicalTrialAgreementId) extends EntityRetrieveCommand[F,  DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaUserPermissions] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}


final case class CreateCtaCmd[F[_]](userId: UserId, permissionsRetriever: CtaBasicPermissionRetriever[F]) extends CtaCreateCommand[F] {
  override def create(): Either[InvoiceError, ClinicalTrialAgreement] = Right(ClinicalTrialAgreement.create(this))
}
