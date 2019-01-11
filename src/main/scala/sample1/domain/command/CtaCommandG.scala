package sample1.domain.command

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId}
import sample1.domain.entity.EntityRepo
import sample1.domain.{InvoiceError, StaleCtaError, UserId}

object CtaCommandG {

}

sealed trait CtaCreateCommandG[F[_]] extends EntityCreateCommandG[F, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement] {
  def action(): Either[InvoiceError, ClinicalTrialAgreement]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

sealed trait CtaUpdateCommandG[F[_]] extends EntityUpdateCommandG[F, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement] {
  def action(cta: ClinicalTrialAgreement): Either[InvoiceError, ClinicalTrialAgreement]

  override def staleF(id: ClinicalTrialAgreementId): InvoiceError = StaleCtaError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

final case class CtaRetrieveCommandG[F[_]](userId: UserId, id: ClinicalTrialAgreementId) extends EntityRetrieveCommandG[F, DomainCommandInput[F], InvoiceError, ClinicalTrialAgreementId, ClinicalTrialAgreement] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, InvoiceError] = input.ctaRepo
}

final case class CreateCtaCmdG[F[_]](userId: UserId) extends CtaCreateCommandG[F] {
  override def action(): Either[InvoiceError, ClinicalTrialAgreement] = Right(ClinicalTrialAgreement.create(this))
}
