package sample1.domain.command

import sample1.domain.cta.CtaRepo
import sample1.domain.entity.{EntityRepo, EntityVersion}
import sample1.domain.invoice.{Invoice, InvoiceAlgebra, InvoiceId}
import sample1.domain.permissions._
import sample1.domain.user.UserId
import sample1.domain.{InvoiceError, InvoiceRepo, StaleInvoiceError}

class DomainCommandInput[F[_]](val invoiceRepo: InvoiceRepo[F],
                               val ctaRepo: CtaRepo[F]
                              ) extends CommandInput

sealed trait InvoiceCreateCommand[F[_]]
  extends EntityCreateCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, InvoiceUserPermissions] {
  def create(): Either[InvoiceError, Invoice]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] =
    input.invoiceRepo
}

sealed trait InvoiceQueryCommand[F[_], R]
  extends EntityQueryCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, R, InvoiceRepo[F], InvoiceUserPermissions] {
  override def extractRepo(input: DomainCommandInput[F]): InvoiceRepo[F] = input.invoiceRepo
}

sealed trait InvoiceUpdateCommand[F[_], CmdType]
  extends EntityUpdateCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, InvoiceUserPermissions] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice]

  override def staleF(id: InvoiceId): InvoiceError = StaleInvoiceError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] =
    input.invoiceRepo
}

final case class InvoiceRetrieveCommand[F[_]](userId: UserId, id: InvoiceId)
  extends EntityRetrieveCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, InvoiceUserPermissions] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] =
    input.invoiceRepo
}


final case class ApproveCmd[F[_]](userId: UserId,
                                  id: InvoiceId,
                                  version: EntityVersion,
                                  permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                 ) extends InvoiceUpdateCommand[F, ApproveCmd[F]] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceAlgebra.approve(invoice, this, permissions)
}

final case class ApproveCmdV2[F[_]](userId: UserId,
                                    id: InvoiceId,
                                    version: EntityVersion,
                                    permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                   ) extends InvoiceUpdateCommand[F, ApproveCmdV2[F]] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceAlgebra.approveV2(invoice, this, permissions)
}

final case class CreateRfiInvoiceCmd[F[_]](userId: UserId,
                                           permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                                          ) extends InvoiceCreateCommand[F] {
  override def create(): Either[InvoiceError, Invoice] = Right(Invoice.createRfiInvoice(this))
}

final case class UpdateRfiCmd[F[_]](userId: UserId,
                                    id: InvoiceId,
                                    version: EntityVersion,
                                    permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                   ) extends InvoiceUpdateCommand[F, UpdateRfiCmd[F]] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceAlgebra.updateRfi(invoice, this, permissions)
}

final case class CreateSiteInvoiceCmd[F[_]](userId: UserId,
                                            permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                                           ) extends InvoiceCreateCommand[F] {
  override def create(): Either[InvoiceError, Invoice] = Right(Invoice.createSiteInvoice(this))
}

final case class FindAll[F[_]](userId: UserId) extends InvoiceQueryCommand[F, Seq[Invoice]] {
  override def query(repo: InvoiceRepo[F]): F[Either[InvoiceError, Seq[Invoice]]] = repo.find()
}
