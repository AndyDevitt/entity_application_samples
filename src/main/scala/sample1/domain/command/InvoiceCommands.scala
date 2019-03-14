package sample1.domain.command

import sample1.domain.cta.CtaRepo
import sample1.domain.entity.{EntityRepo, EntityVersion}
import sample1.domain.invoice.{Invoice, InvoiceAlgebra, InvoiceId}
import sample1.domain.permissions.{EntityPermissionsRetriever, InvoicePermissions}
import sample1.domain.user.UserId
import sample1.domain.{InvoiceError, InvoiceRepo, StaleInvoiceError}

class DomainCommandInput[F[_]](val invoiceRepo: InvoiceRepo[F],
                               val ctaRepo: CtaRepo[F]
                              ) extends CommandInput

sealed trait InvoiceCreateCommand[F[_], H[_]] extends EntityCreateCommand[F, H, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, Set[InvoicePermissions]] {
  def create(): Either[InvoiceError, Invoice]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

sealed trait InvoiceQueryCommand[F[_], H[_], R] extends EntityQueryCommand[F, H, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, R, InvoiceRepo[F], Set[InvoicePermissions]] {
  override def extractRepo(input: DomainCommandInput[F]): InvoiceRepo[F] = input.invoiceRepo
}

sealed trait InvoiceUpdateCommand[F[_], H[_], CmdType] extends EntityUpdateCommand[F, H, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, Set[InvoicePermissions], CmdType] {
  override def action(invoice: Invoice, permissions: Set[InvoicePermissions]): Either[InvoiceError, Invoice]

  override def staleF(id: InvoiceId): InvoiceError = StaleInvoiceError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

final case class InvoiceRetrieveCommand[F[_], H[_]](userId: UserId, id: InvoiceId) extends EntityRetrieveCommand[F, H, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, Set[InvoicePermissions]] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.invoiceRepo
}

final case class ApproveCmd[F[_], H[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommand[F, H, ApproveCmd[F, H]] {
  override def action(invoice: Invoice, permissions: Set[InvoicePermissions]): Either[InvoiceError, Invoice] = InvoiceAlgebra.approve[F, H](invoice, this, permissions)

  override def permissionsRetriever: EntityPermissionsRetriever[H, InvoiceId, Invoice, ApproveCmd[F, H], Set[InvoicePermissions]] = ???
}

final case class ApproveCmdV2[F[_], H[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommand[F, H, ApproveCmdV2[F, H]] {
  override def action(invoice: Invoice, permissions: Set[InvoicePermissions]): Either[InvoiceError, Invoice] = InvoiceAlgebra.approveV2[F, H](invoice, this, permissions)

  override def permissionsRetriever: EntityPermissionsRetriever[H, InvoiceId, Invoice, ApproveCmdV2[F, H], Set[InvoicePermissions]] = ???
}

final case class CreateRfiInvoiceCmd[F[_], H[_]](userId: UserId) extends InvoiceCreateCommand[F, H] {
  override def create(): Either[InvoiceError, Invoice] = Right(Invoice.createRfiInvoiceG(this))
}

final case class UpdateRfiCmd[F[_], H[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommand[F, H, UpdateRfiCmd[F, H]] {
  override def action(invoice: Invoice, permissions: Set[InvoicePermissions]): Either[InvoiceError, Invoice] = InvoiceAlgebra.updateRfi(invoice, this, permissions)

  override def permissionsRetriever: EntityPermissionsRetriever[H, InvoiceId, Invoice, UpdateRfiCmd[F, H], Set[InvoicePermissions]] = ???
}

final case class CreateSiteInvoiceCmd[F[_], H[_]](userId: UserId) extends InvoiceCreateCommand[F, H] {
  override def create(): Either[InvoiceError, Invoice] = Right(Invoice.createSiteInvoice(this))
}

final case class FindAll[F[_], H[_]](userId: UserId) extends InvoiceQueryCommand[F, H, Seq[Invoice]] {
  override def query(repo: InvoiceRepo[F]): F[Either[InvoiceError, Seq[Invoice]]] = repo.find()
}
