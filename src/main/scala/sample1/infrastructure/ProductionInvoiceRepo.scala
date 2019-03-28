package sample1.infrastructure

import cats.Monad
import cats.effect.IO
import cats.syntax.either._
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._

class ProductionInvoiceRepo()(implicit versioned: Versioned[Invoice], codec: EntityRepoCodec[Invoice, Invoice, InvoiceError]) extends InvoiceRepo[IO] {
  self =>
  val entityRepo: InMemoryInvoiceRepo[IO] = new InMemoryInvoiceRepo[IO] {
    override def codec: EntityRepoCodec[Invoice, Invoice, InvoiceError] = self.codec
  }

  override def save(invoice: Invoice)(implicit monad: Monad[IO]): IO[Either[InvoiceError, Invoice]] =
    entityRepo.saveEntity(invoice)

  override def retrieve(id: InvoiceId)(implicit monad: Monad[IO]): IO[Either[InvoiceError, Invoice]] =
    entityRepo.retrieveEntity(id)

  override def find(): IO[Either[InvoiceError, List[Invoice]]] =
    IO.pure(List(
      Invoice.createSiteInvoiceEmpty(),
      Invoice.createSiteInvoiceEmpty(),
      Invoice.createRfiInvoiceEmpty()
    ).asRight[InvoiceError])
}
