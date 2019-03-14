package sample1.domain.permissions

import sample1.domain.cta.{ClinicalTrialAgreement, ClinicalTrialAgreementId}
import sample1.domain.entity.{Entity, EntityId}
import sample1.domain.invoice.{Invoice, InvoiceId}
import sample1.domain.user.UserId

trait Operable

trait Operation

trait Operator

trait PermissionsRetriever[F[_], PermissionsType]

trait BasicPermissionRetriever[F[_], PermissionsType] extends PermissionsRetriever[F, PermissionsType] {
  def retrieve(userId: UserId): F[PermissionsType]
}

trait EntityPermissionsRetriever[F[_], IdType <: EntityId, EntityType <: Entity[IdType], PermissionsType] extends PermissionsRetriever[F, PermissionsType] {
  def retrieve(userId: UserId, entity: EntityType): F[PermissionsType]
}

trait InvoiceBasicPermissionRetriever[F[_]] extends BasicPermissionRetriever[F, InvoiceUserPermissions]

trait InvoiceEntityPermissionRetriever[F[_]] extends EntityPermissionsRetriever[F, InvoiceId, Invoice, InvoiceUserPermissions]

trait CtaBasicPermissionRetriever[F[_]] extends BasicPermissionRetriever[F, CtaUserPermissions]

trait CtaEntityPermissionRetriever[F[_]] extends EntityPermissionsRetriever[F, ClinicalTrialAgreementId, ClinicalTrialAgreement, CtaUserPermissions]
