package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain.entity.EntityRepoManager
import sample1.domain.invoice.{Invoice, InvoiceId}
import sample1.domain.{InvoiceError, StaleInvoiceError}

trait CommandRunner[F[_], G[_], C <: CommandG[G, I, R, E], -I <: CommandInput, R, E] {
  def run(command: C, input: I): F[Either[E, R]]
}

object CommandRunner {

  implicit def invoiceUpdateCommandRunner[F[_], G[_]](implicit transformer: G ~> F, monadF: Monad[G]
                                                     ): CommandRunner[F, G, InvoiceUpdateCommandG[G], DomainCommandInput[G], Invoice, InvoiceError] =
    (command: InvoiceUpdateCommandG[G], input: DomainCommandInput[G]) => {
      println("invoiceUpdate")
      EntityRepoManager.manageUpdate[F, G, DomainCommandInput[G], InvoiceUpdateCommandG[G], InvoiceId, Invoice, InvoiceError](input.invoiceRepo)(command)(command.action, id => StaleInvoiceError(id))
    }

  implicit def invoiceCreateRfiCommandRunner[F[_], G[_]](implicit transformer: G ~> F, monadF: Monad[G]
                                                        ): CommandRunner[F, G, CreateRfiInvoiceCmdG[G], DomainCommandInput[G], Invoice, InvoiceError] =
    (command: CreateRfiInvoiceCmdG[G], input: DomainCommandInput[G]) => {
      println("createRfi")
      EntityRepoManager.manageCreate[F, G, DomainCommandInput[G], CreateRfiInvoiceCmdG[G], InvoiceId, Invoice, InvoiceError](input.invoiceRepo)(command)(() => command.action())
    }

  implicit def invoiceCreateCommandRunner[F[_], G[_]](implicit transformer: G ~> F, monadF: Monad[G]
                                                     ): CommandRunner[F, G, InvoiceCreateCommandG[G], DomainCommandInput[G], Invoice, InvoiceError] =
    (command: InvoiceCreateCommandG[G], input: DomainCommandInput[G]) => {
      println("invoiceCreate")
      EntityRepoManager.manageCreate[F, G, DomainCommandInput[G], InvoiceCreateCommandG[G], InvoiceId, Invoice, InvoiceError](input.invoiceRepo)(command)(() => command.action())
    }

  implicit def invoiceRetrieveCommandRunner[F[_], G[_]](implicit transformer: G ~> F, monadF: Monad[G]
                                                       ): CommandRunner[F, G, InvoiceRetrieveCommandG[G], DomainCommandInput[G], Invoice, InvoiceError] =
    (command: InvoiceRetrieveCommandG[G], input: DomainCommandInput[G]) => {
      println("invoiceRetrieve")
      EntityRepoManager.manageRetrieve[F, G, DomainCommandInput[G], InvoiceRetrieveCommandG[G], InvoiceId, Invoice, InvoiceError](input.invoiceRepo)(command)
    }

}
