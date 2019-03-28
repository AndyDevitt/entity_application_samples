package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceCreateCommand
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.Invoice
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId

object CreateRfiInvoice {

  final case class CreateRfiInvoiceCmd[F[_]](userId: UserId,
                                             permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                                            ) extends InvoiceCreateCommand[F] {
    override def create(permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      Invoice.createRfiInvoice(this, permissions)
  }

}
