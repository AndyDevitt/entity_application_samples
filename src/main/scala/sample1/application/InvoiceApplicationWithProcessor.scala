package sample1.application

import cats.effect.IO
import cats.{Id, Monad, ~>}
import sample1.domain.command._
import sample1.domain.invoice.Invoice
import sample1.domain.{Decoder, InvoiceError, InvoiceRepo}

import scala.concurrent.Future
import scala.language.higherKinds

trait InvoiceApplicationWithProcessor[F[_], G[_]] {

  def repo: InvoiceRepo[G]

  final val input: DomainCommandInput[G] = new DomainCommandInput[G](repo)

  def processCommand[R](cmd: CommandG[G, DomainCommandInput[G], R, InvoiceError])
                       (implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F, decoder: Decoder[InvoiceView, Invoice, InvoiceError]
                       ): F[Either[InvoiceError, R]] =
    cmd.run(input)

}

class ProdApplicationWithProcessor(override val repo: InvoiceRepo[IO]) extends InvoiceApplicationWithProcessor[Future, IO]

class TestApplicationWithProcessor(override val repo: InvoiceRepo[Id]) extends InvoiceApplicationWithProcessor[Id, Id]
