package sample1.domain

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command.{CommandInput, EntityUpdateCommandG, InvoiceCreateCommandG, InvoiceUpdateCommandG}
import sample1.domain.invoice.Invoice

object InvoiceRepoManager {

  def manageCreate[F[_], G[_], I <: CommandInput, A <: InvoiceCreateCommandG[G], R](repo: InvoiceRepo[G])
                                                                                      (cmd: A)
                                                                                      (f: () => Either[InvoiceError, Invoice])
                                                                                      (implicit monadG: Monad[G], transform: G ~> F, decoder: Decoder[R, Invoice, InvoiceError]
                                                                                      ): F[Either[InvoiceError, R]] =
    transform((for {
      updatedInv <- EitherT.fromEither[G](f())
      savedInv <- EitherT(repo.save(updatedInv))
      view <- EitherT.fromEither[G](decoder.decode(savedInv))
    } yield view).value)

  def manageUpdate[F[_], G[_], I <: CommandInput, A <: InvoiceUpdateCommandG[G], R](repo: InvoiceRepo[G])
                                                                                      (cmd: A)
                                                                                      (f: Invoice => Either[InvoiceError, Invoice])
                                                                                      (implicit monadG: Monad[G], transform: G ~> F, decoder: Decoder[R, Invoice, InvoiceError]
                                                                                      ): F[Either[InvoiceError, R]] =
    transform((for {
      inv <- EitherT(repo.retrieve(cmd.id))
      _ <- EitherT.fromEither(checkOptimisticLocking(inv, cmd))
      updatedInv <- EitherT.fromEither[G](f(inv))
      savedInv <- EitherT(repo.save(updatedInv))
      view <- EitherT.fromEither[G](decoder.decode(savedInv))
    } yield view).value)

  private def checkOptimisticLocking[F[_]](invoice: Invoice, cmd: EntityUpdateCommandG[F, _, _, _, _]): Either[InvoiceError, Unit] =
    Either.cond(cmd.version == invoice.version, (), StaleInvoiceError(invoice.id))

}


