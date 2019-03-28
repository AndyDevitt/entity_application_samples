package sample1.domain.invoice.commands

import cats.Monad
import sample1.domain.command.DomainCommandInput
import sample1.domain.command.invoicecommands.InvoiceDomainServiceCommand
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.Invoice
import sample1.domain.permissions.{InvoiceBasicPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId

object ExampleDomainService {

  final case class ExampleDomainServiceCmd[F[_]](userId: UserId,
                                                 permissionsRetriever: InvoiceBasicPermissionRetriever[F]
                                                ) extends InvoiceDomainServiceCommand[F, Invoice] {

    override def action(input: DomainCommandInput[F], permissions: InvoiceUserPermissions)
                       (implicit monadF: Monad[F]
                       ): F[Either[InvoiceError, Invoice]] =
      ExampleDomainService().run(input, permissions)
  }

  final case class ExampleDomainService[F[_]]() {
    def run(input: DomainCommandInput[F], permissions: InvoiceUserPermissions)
           (implicit monadF: Monad[F]): F[Either[InvoiceError, Invoice]] =
      monadF.pure(
        if (permissions.has(InvoicePermissions.RunDomainServices)) {
          Right(Invoice.createSiteInvoiceEmpty())
        } else {
          Left(InvoiceError.InsufficientPermissions("Not enough permissions to run domain services, sorry"))
        }
      )
  }

}
