package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain.entity.EntityRepoManager
import sample1.domain.invoice.{Invoice, InvoiceId}
import sample1.domain.{InvoiceError, StaleInvoiceError}

trait CommandRunner[F[_], G[_], C <: CommandG[F, I, R, E], -I <: CommandInput, R, E] {
  def run(command: C, input: I): G[Either[E, R]]
}

object CommandRunner {
  implicit def invoiceUpdateCommandRunner[F[_], G[_]](implicit transformer: F ~> G, monadF: Monad[F]
                                                     ): CommandRunner[F, G, InvoiceUpdateCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] =
    new CommandRunner[F, G, InvoiceUpdateCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] {
      override def run(command: InvoiceUpdateCommandG[F], input: DomainCommandInput[F]): G[Either[InvoiceError, Invoice]] =
        EntityRepoManager.manageUpdate[G, F, DomainCommandInput[F], InvoiceUpdateCommandG[F], InvoiceId, Invoice, InvoiceError](input.repo)(command)(command.action, id => StaleInvoiceError(id))
    }

//  implicit def invoiceCreateCommandRunner[F[_], G[_]](implicit transformer: F ~> G, monadF: Monad[F]
//                                                     ): CommandRunner[F, G, InvoiceCreateCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] =
//    new CommandRunner[F, G, InvoiceCreateCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] {
//      override def run(command: InvoiceCreateCommandG[F], input: DomainCommandInput[F]): G[Either[InvoiceError, Invoice]] =
//        EntityRepoManager.manageCreate[G, F, DomainCommandInput[F], InvoiceCreateCommandG[F], InvoiceId, Invoice, InvoiceError](input.repo)(command)(() => command.action())
//    }

  implicit def invoiceRetrieveCommandRunner[F[_], G[_]](implicit transformer: F ~> G, monadF: Monad[F]
                                                       ): CommandRunner[F, G, InvoiceRetrieveCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] =
    new CommandRunner[F, G, InvoiceRetrieveCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] {
      override def run(command: InvoiceRetrieveCommandG[F], input: DomainCommandInput[F]): G[Either[InvoiceError, Invoice]] =
        EntityRepoManager.manageRetrieve[G, F, DomainCommandInput[F], InvoiceRetrieveCommandG[F], InvoiceId, Invoice, InvoiceError](input.repo)(command)
    }

  implicit def invoiceCreateRfiCommandRunner[F[_], G[_]](implicit transformer: F ~> G, monadF: Monad[F]
                                                        ): CommandRunner[F, G, CreateRfiInvoiceCmdG[F], DomainCommandInput[F], Invoice, InvoiceError] =
    new CommandRunner[F, G, CreateRfiInvoiceCmdG[F], DomainCommandInput[F], Invoice, InvoiceError] {
      override def run(command: CreateRfiInvoiceCmdG[F], input: DomainCommandInput[F]): G[Either[InvoiceError, Invoice]] =
        EntityRepoManager.manageCreate[G, F, DomainCommandInput[F], CreateRfiInvoiceCmdG[F], InvoiceId, Invoice, InvoiceError](input.repo)(command)(() => command.action())
    }
}
