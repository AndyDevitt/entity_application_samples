package sample1.infrastructure

import cats.syntax.either._
import cats.{Id, Monad}
import sample1.domain.entity.{EntityRepoCodec, EntityRepoImpl, PersistenceRepo, Versioned}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.{Invoice, InvoiceId, InvoiceRepo}

class TestInvoiceRepo(override val persistenceRepo: PersistenceRepo[Id, InvoiceError, Invoice, InvoiceId],
                      override val codec: EntityRepoCodec[Invoice, Invoice, InvoiceError])
                     (implicit versioned: Versioned[Invoice])
  extends InvoiceRepo[Id]
    with EntityRepoImpl[Id, InvoiceId, Invoice, InvoiceError, Invoice, InvoiceId] {
  self =>

  override def save(invoice: Invoice)(implicit monad: Monad[Id]): Id[Either[InvoiceError, Invoice]] =
    saveEntity(invoice)

  override def retrieve(id: InvoiceId)(implicit monad: Monad[Id]): Id[Either[InvoiceError, Invoice]] =
    retrieveEntity(id)

  override def find(): Id[Either[InvoiceError, Seq[Invoice]]] =
    Seq(
      Invoice.createSiteInvoiceEmpty(),
      Invoice.createSiteInvoiceEmpty(),
      Invoice.createRfiInvoiceEmpty()
    ).asRight[InvoiceError]
}
