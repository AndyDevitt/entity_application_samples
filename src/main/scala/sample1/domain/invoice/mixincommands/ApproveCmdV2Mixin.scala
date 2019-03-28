package sample1.domain.invoice.mixincommands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.mixinstatemachine.InvoiceBehaviour
import sample1.domain.invoice.{Invoice, InvoiceAction, InvoiceId}
import sample1.domain.permissions.{InvoiceEntityPermissionRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId

final case class ApproveCmdV2Mixin[F[_]](userId: UserId,
                                         id: InvoiceId,
                                         version: EntityVersion,
                                         permissionsRetriever: InvoiceEntityPermissionRetriever[F])
  extends InvoiceUpdateCommand[F, ApproveCmdV2Mixin[F], InvoiceAction.Approve.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceBehaviour(invoice).process(this, permissions)

  override def associatedAction: InvoiceAction.Approve.type = InvoiceAction.Approve
}
