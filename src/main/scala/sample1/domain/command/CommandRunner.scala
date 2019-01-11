package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain.InvoiceError
import sample1.domain.invoice.Invoice

trait CommandRunner[F[_], G[_], C <: CommandG[F, I, R, E], -I <: CommandInput, R, E] {
  def run(command: C, input: I): G[Either[E, R]]
}

object CommandRunner {

  implicit def invoiceUpdateCommandRunner[F[_], G[_]](implicit transformer: F ~> G, monadF: Monad[F]
                                                     ): CommandRunner[F, G, InvoiceUpdateCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] =
    ??? //(command: InvoiceUpdateCommandG[F], input: DomainCommandInput[F]) => InvoiceRepoManager.manageUpdate(input.repo)(command)(command.action)

  //  implicit val idInvoiceUpdateCommandRunner = invoiceUpdateCommandRunner[Id,Id]
}
