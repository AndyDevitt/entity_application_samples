package sample1.domain.command

import sample1.domain.{InvoiceError, StaleError, UserId}
import sample1.domain.entity.{EntityRepo, EntityVersion}
import sample1.domain.invoice.{Invoice, InvoiceAlgebra, InvoiceId}

object InvoiceCommandG {

}

sealed trait InvoiceCreateCommandG[F[_]] extends EntityCreateCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(): Either[InvoiceError, Invoice]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.repo
}

sealed trait InvoiceUpdateCommandG[F[_]] extends EntityUpdateCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(invoice: Invoice): Either[InvoiceError, Invoice]

  override def staleF(id: InvoiceId): InvoiceError = StaleError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.repo
}

final case class ApproveCmdG[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.approveG[F](invoice, this)
}

final case class CreateRfiInvoiceCmdG[F[_]](userId: UserId) extends InvoiceCreateCommandG[F] {
  override def action(): Either[InvoiceError, Invoice] = Right(Invoice.createRfiInvoice(this))
}

final case class UpdateRfiCmdG[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.updateRfiG(invoice, this)
}
