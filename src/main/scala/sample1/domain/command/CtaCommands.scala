package sample1.domain.command

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId, CtaAction}
import sample1.domain.entity.EntityRepo
import sample1.domain.errors.CtaError
import sample1.domain.permissions.{CtaBasicPermissionRetriever, CtaEntityPermissionRetriever, CtaUserPermissions}
import sample1.domain.user.UserId

sealed trait CtaPermissions

sealed trait CtaCreateCommand[F[_]]
  extends EntityCreateCommand[
    F,
    DomainCommandInput[F],
    CtaError,
    ClinicalTrialAgreementId,
    ClinicalTrialAgreement,
    CtaUserPermissions] {
  override def create(permissions: CtaUserPermissions): Either[CtaError, ClinicalTrialAgreement]

  override def extractRepo(input: DomainCommandInput[F]
                          ): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaError] =
    input.ctaRepo
}

sealed trait CtaUpdateCommand[F[_], H[_], CmdType]
  extends EntityUpdateCommand[
    F,
    DomainCommandInput[F],
    CtaError,
    ClinicalTrialAgreementId,
    ClinicalTrialAgreement,
    CtaUserPermissions,
    CtaAction] {
  override def staleF(id: ClinicalTrialAgreementId): CtaError = CtaError.StaleCtaError(id)

  override def extractRepo(input: DomainCommandInput[F]
                          ): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaError] =
    input.ctaRepo
}


final case class CtaRetrieveCommand[F[_]](userId: UserId,
                                          id: ClinicalTrialAgreementId,
                                          permissionsRetriever: CtaEntityPermissionRetriever[F])
  extends EntityRetrieveCommand[
    F,
    DomainCommandInput[F],
    CtaError,
    ClinicalTrialAgreementId,
    ClinicalTrialAgreement,
    CtaUserPermissions] {
  override def extractRepo(input: DomainCommandInput[F]
                          ): EntityRepo[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaError] =
    input.ctaRepo

  override def checkMinimumPermissions(permissions: CtaUserPermissions): Either[CtaError, Unit] = Right(())
}

final case class CreateCtaCmd[F[_]](userId: UserId, permissionsRetriever: CtaBasicPermissionRetriever[F])
  extends CtaCreateCommand[F] {
  override def create(permissions: CtaUserPermissions): Either[CtaError, ClinicalTrialAgreement] =
    Right(ClinicalTrialAgreement.create(this))

  override def checkMinimumPermissions(permissions: CtaUserPermissions): Either[CtaError, Unit] = Right(())
}
