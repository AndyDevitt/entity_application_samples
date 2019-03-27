package sample1.domain.command.invoicecommands

import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.invoice.mixinstatemachine.InvoiceBehaviour
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoiceEntityPermissionRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId

/**
  * Concrete command implementations
  */

final case class ApproveCmdMixin[F[_]](userId: UserId,
                                       id: InvoiceId,
                                       version: EntityVersion,
                                       permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                      ) extends InvoiceUpdateCommand[F, ApproveCmdMixin[F], InvoiceAction.Approve.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceBehaviour(invoice).process(this, permissions)

  override def associatedAction: InvoiceAction.Approve.type = InvoiceAction.Approve
}

final case class ApproveCmdV2Mixin[F[_]](userId: UserId,
                                         id: InvoiceId,
                                         version: EntityVersion,
                                         permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                        ) extends InvoiceUpdateCommand[F, ApproveCmdV2Mixin[F], InvoiceAction.Approve.type] {
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

final case class UpdateRfiCmdMixin[F[_]](userId: UserId,
                                         id: InvoiceId,
                                         version: EntityVersion,
                                         permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                        ) extends InvoiceUpdateCommand[F, UpdateRfiCmdMixin[F], InvoiceAction.UpdateRfi.type] {
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
