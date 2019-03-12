package sample1.infrastructure

import cats.syntax.either._
import cats.{Id, Monad}
import sample1.domain.command.{CreateRfiInvoiceCmd, CreateSiteInvoiceCmd}
import sample1.domain.entity.{EntityRepoCodec, Versioned}
import sample1.domain.invoice.{Invoice, InvoiceId}
import sample1.domain.{InvoiceError, InvoiceRepo, UserId}

class TestInvoiceRepo()(implicit versioned: Versioned[Invoice], codec: EntityRepoCodec[Invoice, Invoice, InvoiceError]) extends InvoiceRepo[Id] {
  val entityRepo: InMemoryInvoiceRepo[Id] = new InMemoryInvoiceRepo[Id] {}

  override def save(invoice: Invoice)(implicit monad: Monad[Id]): Id[Either[InvoiceError, Invoice]] =
    entityRepo.saveEntity(invoice)

  override def retrieve(id: InvoiceId)(implicit monad: Monad[Id]): Id[Either[InvoiceError, Invoice]] =
    entityRepo.retrieveEntity(id)

  override def find(): Id[Either[InvoiceError, Seq[Invoice]]] =
    Seq(
      Invoice.createSiteInvoice(CreateSiteInvoiceCmd(UserId())),
      Invoice.createSiteInvoice(CreateSiteInvoiceCmd(UserId())),
      Invoice.createRfiInvoiceG(CreateRfiInvoiceCmd(UserId()))
    ).asRight[InvoiceError]
}
