package sample1.domain.command

import sample1.domain.cta.CtaRepo
import sample1.domain.entity.{EntityRepo, EntityVersion}
import sample1.domain.invoice.{Invoice, InvoiceAlgebra, InvoiceId}
import sample1.domain.{InvoiceError, InvoiceRepo, StaleInvoiceError, UserId}

object InvoiceCommandG {

}

class DomainCommandInput[F[_]](val invoiceRepo: InvoiceRepo[F],
                               val ctaRepo: CtaRepo[F]
                              ) extends CommandInput

sealed trait InvoiceCreateCommandG[F[_]] extends EntityCreateCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(): Either[InvoiceError, Invoice]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

sealed trait InvoiceQueryCommandG[F[_], R] extends EntityQueryCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, R, InvoiceRepo[F]] {
  override def extractRepo(input: DomainCommandInput[F]): InvoiceRepo[F] = input.invoiceRepo
}

sealed trait InvoiceUpdateCommandG[F[_]] extends EntityUpdateCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(invoice: Invoice): Either[InvoiceError, Invoice]

  override def staleF(id: InvoiceId): InvoiceError = StaleInvoiceError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

final case class InvoiceRetrieveCommandG[F[_]](userId: UserId, id: InvoiceId) extends EntityRetrieveCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

final case class ApproveCmdG[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.approveG[F](invoice, this)
}

final case class ApproveCmd3G[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.approve3G[F](invoice, this)
}

final case class CreateRfiInvoiceCmdG[F[_]](userId: UserId) extends InvoiceCreateCommandG[F] {
  override def action(): Either[InvoiceError, Invoice] = Right(Invoice.createRfiInvoiceG(this))
}

final case class UpdateRfiCmdG[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.updateRfiG(invoice, this)
}

final case class CreateSiteInvoiceCmdG[F[_]](userId: UserId) extends InvoiceCreateCommandG[F] {
  override def action(): Either[InvoiceError, Invoice] = Right(Invoice.createSiteInvoice(this))
}

final case class FindAllG[F[_]](userId: UserId) extends InvoiceQueryCommandG[F, Seq[Invoice]] {
  override def query(repo: InvoiceRepo[F]): F[Either[InvoiceError, Seq[Invoice]]] = repo.find()
}
