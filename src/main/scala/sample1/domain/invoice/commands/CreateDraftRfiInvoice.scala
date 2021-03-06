package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceCreateCommand
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.Invoice
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId

object CreateDraftRfiInvoice {

  final case class CreateDraftRfiInvoice[F[_]](userId: UserId,
                                               permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                                              ) extends InvoiceCreateCommand[F] {
    override def create(permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      Invoice.createDraftRfiInvoice(this, permissions)
  }

}

