package sample1.infrastructure

import cats.{Id, Monad}
import sample1.domain.entity.Versioned
import sample1.domain.invoice.{Invoice, InvoiceId}
import sample1.domain.{Codec, InvoiceError, InvoiceRepo}

class TestInvoiceRepo()(implicit versioned: Versioned[Invoice], codec: Codec[Invoice, Invoice, InvoiceError]) extends InvoiceRepo[Id] {
  val entityRepo: InMemoryInvoiceRepo[Id] = new InMemoryInvoiceRepo[Id] {}

  override def save(invoice: Invoice)(implicit monad: Monad[Id]): Id[Either[InvoiceError, Invoice]] =
    entityRepo.saveEntity(invoice)

  override def retrieve(id: InvoiceId)(implicit monad: Monad[Id]): Id[Either[InvoiceError, Invoice]] =
    entityRepo.retrieveEntity(id)
}