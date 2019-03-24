package sample1.domain.command.invoicecommands

import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.invoice.mixinstatemachine.InvoiceBehaviour
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoiceEntityPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId

/**
  * Concrete command implementations
  */

final case class ApproveCmd[F[_]](userId: UserId,
                                  id: InvoiceId,
                                  version: EntityVersion,
                                  permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                 ) extends InvoiceUpdateCommand[F, ApproveCmd[F], InvoiceAction.Approve.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceAlgebra.Approve3(invoice, this, permissions)

  override def associatedAction: InvoiceAction.Approve.type = InvoiceAction.Approve

  override def checkMinimumPermissions(permissions: InvoiceUserPermissions): Either[InvoiceError, Unit] =
    Either.cond(
      permissions.hasReadPermission && permissions.has(InvoicePermissions.Approve),
      (),
      InvoiceError.InsufficientPermissions())
}

final case class ApproveCmdMixin[F[_]](userId: UserId,
                                       id: InvoiceId,
                                       version: EntityVersion,
                                       permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                      ) extends InvoiceUpdateCommand[F, ApproveCmd[F], InvoiceAction.Approve.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceBehaviour(invoice).process(this, permissions)

  override def associatedAction: InvoiceAction.Approve.type = InvoiceAction.Approve

  override def checkMinimumPermissions(permissions: InvoiceUserPermissions): Either[InvoiceError, Unit] =
    Either.cond(
      permissions.hasReadPermission && permissions.has(InvoicePermissions.Approve),
      (),
      InvoiceError.InsufficientPermissions())
}

final case class ApproveCmdV2[F[_]](userId: UserId,
                                    id: InvoiceId,
                                    version: EntityVersion,
                                    permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                   ) extends InvoiceUpdateCommand[F, ApproveCmdV2[F], InvoiceAction.Approve.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceAlgebra.ApproveV2().process(invoice, this, permissions)

  override def associatedAction: InvoiceAction.Approve.type = InvoiceAction.Approve
}

final case class ApproveCmdV2Mixin[F[_]](userId: UserId,
                                         id: InvoiceId,
                                         version: EntityVersion,
                                         permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                        ) extends InvoiceUpdateCommand[F, ApproveCmdV2[F], InvoiceAction.Approve.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceBehaviour(invoice).process(this, permissions)

  override def associatedAction: InvoiceAction.Approve.type = InvoiceAction.Approve
}

final case class CreateRfiInvoiceCmd[F[_]](userId: UserId,
                                           permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                                          ) extends InvoiceCreateCommand[F] {
  override def create(permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    Invoice.createRfiInvoice(this, permissions)
}

final case class UpdateRfiCmd[F[_]](userId: UserId,
                                    id: InvoiceId,
                                    version: EntityVersion,
                                    permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                   ) extends InvoiceUpdateCommand[F, UpdateRfiCmd[F], InvoiceAction.UpdateRfi.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceAlgebra.UpdateRfi().process(invoice, this, permissions)

  override def associatedAction: InvoiceAction.UpdateRfi.type = InvoiceAction.UpdateRfi
}

final case class UpdateRfiCmdMixin[F[_]](userId: UserId,
                                         id: InvoiceId,
                                         version: EntityVersion,
                                         permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                        ) extends InvoiceUpdateCommand[F, UpdateRfiCmd[F], InvoiceAction.UpdateRfi.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceBehaviour(invoice).process(this, permissions)

  override def associatedAction: InvoiceAction.UpdateRfi.type = InvoiceAction.UpdateRfi
}

final case class CreateSiteInvoiceCmd[F[_]](userId: UserId,
                                            permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                                           ) extends InvoiceCreateCommand[F] {
  override def create(permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    Invoice.createSiteInvoice(this, permissions)
}

final case class FindAll[F[_]](userId: UserId,
                               permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                              ) extends InvoiceQueryCommand[F, Seq[Invoice]] {
  override def query(repo: InvoiceRepo[F]): F[Either[InvoiceError, Seq[Invoice]]] = repo.find()
}
