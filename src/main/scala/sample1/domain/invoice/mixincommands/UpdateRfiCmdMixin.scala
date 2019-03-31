package sample1.domain.invoice.mixincommands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.mixinstatemachine.InvoiceBehaviour
import sample1.domain.invoice.{Invoice, InvoiceAction, InvoiceId}
import sample1.domain.permissions.{InvoiceEntityPermissionRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId

final case class UpdateRfiCmdMixin[F[_]](userId: UserId,
                                         id: InvoiceId,
                                         version: EntityVersion,
                                         permissionsRetriever: InvoiceEntityPermissionRetriever[F])
  extends InvoiceUpdateCommand[F, InvoiceAction.UpdateRfi.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    InvoiceBehaviour(invoice).process(this, permissions)
}
