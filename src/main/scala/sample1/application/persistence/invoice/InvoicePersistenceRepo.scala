package sample1.application.persistence.invoice

import sample1.domain.entity.{EntityId, PersistenceRepo, VersionedEntity}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.Invoice

trait InvoicePersistenceRepo[F[_], PersistenceIdType <: EntityId, PersistenceType <: VersionedEntity[PersistenceIdType]]
  extends PersistenceRepo[F, PersistenceIdType, PersistenceType, InvoiceError] {
  def find(): F[Either[InvoiceError, Seq[Invoice]]]
}
