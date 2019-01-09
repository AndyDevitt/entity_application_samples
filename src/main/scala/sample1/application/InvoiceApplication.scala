package sample1.application

import cats.data.EitherT
import cats.effect.IO
import cats.{Id, Monad, ~>}
import sample1.domain.command._
import sample1.domain.invoice._
import sample1.domain.{InvoiceRepo, _}

import scala.concurrent.Future
import scala.language.{higherKinds, postfixOps}

trait InvoiceApplication[F[_], G[_]] {

  def repo: InvoiceRepo[G]

  type Result = F[Either[InvoiceError, Invoice]]

  private def manageUpdate[A <: EntityUpdateCommand[InvoiceId], B](cmd: A)
                                                                  (f: (Invoice, A) => Either[InvoiceError, Invoice])
                                                                  (implicit monadF: Monad[F], monadG: Monad[G], transform: G ~> F, decoder: Decoder[B, Invoice, InvoiceError]
                                                                  ): F[Either[InvoiceError, B]] =
    transform((for {
      inv <- EitherT(repo.retrieve(cmd.id))
      updatedInv <- EitherT.fromEither[G](f(inv, cmd))
      savedInv <- EitherT(repo.save(updatedInv))
      view <- EitherT.fromEither[G](decoder.decode(savedInv))
    } yield view).value)

  private def manageCreate[A <: EntityCommand, B](cmd: A)
                                                 (f: A => Either[InvoiceError, Invoice])
                                                 (implicit monadF: Monad[F], monadG: Monad[G], transform: G ~> F, decoder: Decoder[B, Invoice, InvoiceError]
                                                 ): F[Either[InvoiceError, B]] =
    transform((for {
      createdInv <- EitherT.fromEither[G](f(cmd))
      savedInv <- EitherT(repo.save(createdInv))
      view <- EitherT.fromEither[G](decoder.decode(savedInv))
    } yield view).value)

  def createRfiInvoice(cmd: CreateRfiInvoiceCmd)(implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F): Result =
    manageCreate(cmd)(_ => Right(Invoice.createRfiInvoice(cmd)))

  def approveInvoice(cmd: ApproveCmd)(implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F): F[Either[InvoiceError, Invoice]] =
    manageUpdate(cmd)(InvoiceAlgebra.approve)

  def approveInvoiceAndTrans(cmd: ApproveCmd)(implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F, decoder: Decoder[InvoiceView, Invoice, InvoiceError]): F[Either[InvoiceError, InvoiceView]] =
    manageUpdate(cmd)(InvoiceAlgebra.approve)

  def approveInvoice2(cmd: ApproveCmd)(implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F): F[Either[InvoiceError, Invoice]] =
    manageUpdate(cmd)(InvoiceAlgebra.approve2)

  def updateRfi(cmd: UpdateRfiCmd)(implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F): F[Either[InvoiceError, Invoice]] =
    manageUpdate(cmd)(InvoiceAlgebra.updateRfi)

  def updateRfiAndTrans(cmd: UpdateRfiCmd)(implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F, decoder: Decoder[InvoiceView, Invoice, InvoiceError]): F[Either[InvoiceError, InvoiceView]] =
    manageUpdate(cmd)(InvoiceAlgebra.updateRfi)

  val input: DomainCommandInput[G] = new DomainCommandInput[G](repo)

  def processCommand[R](cmd: CommandG[G, DomainCommandInput[G], R, InvoiceError])
                       (implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F, decoder: Decoder[InvoiceView, Invoice, InvoiceError]
                       ): F[Either[InvoiceError, R]] =
    cmd.run(input)


}

class ProdApplication(override val repo: InvoiceRepo[IO]) extends InvoiceApplication[Future, IO]

class TestApplication(override val repo: InvoiceRepo[Id]) extends InvoiceApplication[Id, Id]
