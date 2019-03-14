package sample1.domain

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command.{CommandInput, EntityUpdateCommand, InvoiceCreateCommand, InvoiceUpdateCommand}
import sample1.domain.invoice.Invoice

object InvoiceRepoManager {

  // TODO update the types here if this is needed
//  def manageCreate[F[_], G[_], I <: CommandInput, A <: InvoiceCreateCommand[G], R](repo: InvoiceRepo[G])
//                                                                                  (cmd: A)
//                                                                                  (f: () => Either[InvoiceError, Invoice])
//                                                                                  (implicit monadG: Monad[G], transform: G ~> F, decoder: Decoder[R, Invoice, InvoiceError]
//                                                                                      ): F[Either[InvoiceError, R]] =
//    transform((for {
//      updatedInv <- EitherT.fromEither[G](f())
//      savedInv <- EitherT(repo.save(updatedInv))
//      view <- EitherT.fromEither[G](decoder.decode(savedInv))
//    } yield view).value)
//
//  def manageUpdate[F[_], G[_], I <: CommandInput, A <: InvoiceUpdateCommand[G], R](repo: InvoiceRepo[G])
//                                                                                  (cmd: A)
//                                                                                  (f: Invoice => Either[InvoiceError, Invoice])
//                                                                                  (implicit monadG: Monad[G], transform: G ~> F, decoder: Decoder[R, Invoice, InvoiceError]
//                                                                                      ): F[Either[InvoiceError, R]] =
//    transform((for {
//      inv <- EitherT(repo.retrieve(cmd.id))
//      _ <- EitherT.fromEither(checkOptimisticLocking(inv, cmd))
//      updatedInv <- EitherT.fromEither[G](f(inv))
//      savedInv <- EitherT(repo.save(updatedInv))
//      view <- EitherT.fromEither[G](decoder.decode(savedInv))
//    } yield view).value)
//
//  private def checkOptimisticLocking[F[_]](invoice: Invoice, cmd: EntityUpdateCommand[F, _, _, _, _]): Either[InvoiceError, Unit] =
//    Either.cond(cmd.version == invoice.version, (), StaleInvoiceError(invoice.id))

}


