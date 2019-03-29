package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceEntityQueryCommand
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.{Invoice, InvoiceRepo}
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId

final case class FindAllEntities[F[_]](userId: UserId,
                                       permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                                      ) extends InvoiceEntityQueryCommand[F] {
  override def query(repo: InvoiceRepo[F],
                     permissions: InvoiceUserPermissions
                    ): F[Either[InvoiceError, List[Invoice]]] =
    repo.find()
}
