package sample1.infrastructure

import cats.Monad
import cats.effect.IO
import cats.syntax.either._
import sample1.domain._
import sample1.domain.command.{CreateRfiInvoiceCmdG, CreateSiteInvoiceCmdG}
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.invoice._

class ProductionInvoiceRepo()(implicit versioned: Versioned[Invoice], codec: EntityRepoCodec[Invoice, Invoice, InvoiceError]) extends InvoiceRepo[IO] {
  val entityRepo: InMemoryInvoiceRepo[IO] = new InMemoryInvoiceRepo[IO] {}

  override def save(invoice: Invoice)(implicit monad: Monad[IO]): IO[Either[InvoiceError, Invoice]] =
    entityRepo.saveEntity(invoice)

  override def retrieve(id: InvoiceId)(implicit monad: Monad[IO]): IO[Either[InvoiceError, Invoice]] =
    entityRepo.retrieveEntity(id)

  override def find(): IO[Either[InvoiceError, Seq[Invoice]]] =
    IO.pure(Seq(
      Invoice.createSiteInvoice(CreateSiteInvoiceCmdG(UserId())),
      Invoice.createSiteInvoice(CreateSiteInvoiceCmdG(UserId())),
      Invoice.createRfiInvoiceG(CreateRfiInvoiceCmdG(UserId()))
    ).asRight[InvoiceError])
}
