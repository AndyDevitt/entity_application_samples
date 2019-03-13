package sample1.domain.command

import sample1.domain.cta.CtaRepo
import sample1.domain.entity.{EntityRepo, EntityVersion}
import sample1.domain.invoice.{Invoice, InvoiceAlgebra, InvoiceId}
import sample1.domain.user.UserId
import sample1.domain.{InvoiceError, InvoiceRepo, StaleInvoiceError}

class DomainCommandInput[F[_]](val invoiceRepo: InvoiceRepo[F],
                               val ctaRepo: CtaRepo[F]
                              ) extends CommandInput

sealed trait InvoiceCreateCommand[F[_]] extends EntityCreateCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def create(): Either[InvoiceError, Invoice]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

sealed trait InvoiceQueryCommand[F[_], R] extends EntityQueryCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, R, InvoiceRepo[F]] {
  override def extractRepo(input: DomainCommandInput[F]): InvoiceRepo[F] = input.invoiceRepo
}

sealed trait InvoiceUpdateCommand[F[_]] extends EntityUpdateCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(invoice: Invoice): Either[InvoiceError, Invoice]

  override def staleF(id: InvoiceId): InvoiceError = StaleInvoiceError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

final case class InvoiceRetrieveCommand[F[_]](userId: UserId, id: InvoiceId) extends EntityRetrieveCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

final case class ApproveCmd[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommand[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.approve[F](invoice, this)
}

final case class ApproveCmdV2[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommand[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.approveV2[F](invoice, this)
}

final case class CreateRfiInvoiceCmd[F[_]](userId: UserId) extends InvoiceCreateCommand[F] {
  override def create(): Either[InvoiceError, Invoice] = Right(Invoice.createRfiInvoiceG(this))
}

final case class UpdateRfiCmd[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommand[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.updateRfi(invoice, this)
}

final case class CreateSiteInvoiceCmd[F[_]](userId: UserId) extends InvoiceCreateCommand[F] {
  override def create(): Either[InvoiceError, Invoice] = Right(Invoice.createSiteInvoice(this))
}

final case class FindAll[F[_]](userId: UserId) extends InvoiceQueryCommand[F, Seq[Invoice]] {
  override def query(repo: InvoiceRepo[F]): F[Either[InvoiceError, Seq[Invoice]]] = repo.find()
}
