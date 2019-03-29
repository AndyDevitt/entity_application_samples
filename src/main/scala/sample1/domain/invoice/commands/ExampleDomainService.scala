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
                                                 permissionsRetriever: InvoiceBasicPermissionRetriever[F],
                                                 flag: Boolean
                                                ) extends InvoiceDomainServiceCommand[F, Invoice] {

    override def action(input: DomainCommandInput[F], permissions: InvoiceUserPermissions)
                       (implicit monadF: Monad[F]
                       ): F[Either[InvoiceError, Invoice]] =
      input.service.run(input, permissions, this)
  }

  trait ExampleDomainService[F[_]] {
    def run(input: DomainCommandInput[F], permissions: InvoiceUserPermissions, cmd: ExampleDomainServiceCmd[F])
           (implicit monadF: Monad[F]
           ): F[Either[InvoiceError, Invoice]]
  }

  final case class ExampleDomainServiceImpl[F[_]]() extends ExampleDomainService[F] {
    def run(input: DomainCommandInput[F], permissions: InvoiceUserPermissions, cmd: ExampleDomainServiceCmd[F])
           (implicit monadF: Monad[F]): F[Either[InvoiceError, Invoice]] =
      monadF.pure(
        if (permissions.has(InvoicePermissions.RunDomainServices)) {
          if (cmd.flag) {
            Right(Invoice.createSiteInvoiceEmpty())
          } else {
            Left(InvoiceError.CannotRunService())
          }
        } else {
          Left(InvoiceError.InsufficientPermissions("Not enough permissions to run domain services, sorry"))
        }
      )
  }

}
