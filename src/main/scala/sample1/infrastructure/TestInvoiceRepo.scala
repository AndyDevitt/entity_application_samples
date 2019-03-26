package sample1.infrastructure

import cats.syntax.either._
import cats.{Id, Monad}
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.{Invoice, InvoiceId, InvoiceRepo}

class TestInvoiceRepo()(implicit versioned: Versioned[Invoice], codec: EntityRepoCodec[Invoice, Invoice, InvoiceError]) extends InvoiceRepo[Id] {
  self =>
  val entityRepo: InMemoryInvoiceRepo[Id] = new InMemoryInvoiceRepo[Id] {
    override def codec: EntityRepoCodec[Invoice, Invoice, InvoiceError] = self.codec
  }

  override def save(invoice: Invoice)(implicit monad: Monad[Id]): Id[Either[InvoiceError, Invoice]] =
    entityRepo.saveEntity(invoice)

  override def retrieve(id: InvoiceId)(implicit monad: Monad[Id]): Id[Either[InvoiceError, Invoice]] =
    entityRepo.retrieveEntity(id)

  override def find(): Id[Either[InvoiceError, Seq[Invoice]]] =
    Seq(
      Invoice.createSiteInvoiceEmpty(),
      Invoice.createSiteInvoiceEmpty(),
      Invoice.createRfiInvoiceEmpty()
    ).asRight[InvoiceError]
}
