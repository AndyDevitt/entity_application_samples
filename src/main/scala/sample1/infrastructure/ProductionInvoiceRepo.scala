package sample1.infrastructure

import cats.Monad
import cats.effect.IO
import sample1.domain._
import sample1.domain.entity.Versioned
import sample1.domain.invoice._

class ProductionInvoiceRepo()(implicit versioned: Versioned[Invoice], codec: Codec[Invoice, Invoice, InvoiceError]) extends InvoiceRepo[IO] {
  val entityRepo: InMemoryInvoiceRepo[IO] = new InMemoryInvoiceRepo[IO] {}

  override def save(invoice: Invoice)(implicit monad: Monad[IO]): IO[Either[InvoiceError, Invoice]] =
    entityRepo.saveEntity(invoice)

  override def retrieve(id: InvoiceId)(implicit monad: Monad[IO]): IO[Either[InvoiceError, Invoice]] =
    entityRepo.retrieveEntity(id)
}