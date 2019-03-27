package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceQueryCommand
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.{Invoice, InvoiceRepo}
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId

final case class FindAll[F[_]](userId: UserId,
                               permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                              ) extends InvoiceQueryCommand[F, Seq[Invoice]] {
  override def query(repo: InvoiceRepo[F],
                     permissions: InvoiceUserPermissions
                    ): F[Either[InvoiceError, Seq[Invoice]]] =
    repo.find()
}
