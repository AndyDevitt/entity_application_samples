package sample1.application

import cats.effect.IO
import cats.{Id, Monad, ~>}
import sample1.domain.command._
import sample1.domain.cta.CtaRepo
import sample1.domain.{Decoder, InvoiceError, InvoiceRepo}

import scala.concurrent.Future
import scala.language.higherKinds

trait InvoiceApplicationWithProcessor[F[_], G[_]] {

  def repo: InvoiceRepo[G]

  def ctaRepo: CtaRepo[G]

  final val input: DomainCommandInput[G] = new DomainCommandInput[G](repo, ctaRepo)

  def processCommand[R, T](cmd: CommandG[G, DomainCommandInput[G], R, InvoiceError])
                          (implicit monadF: Monad[F], monadG: Monad[G], trans: G ~> F, decoder: Decoder[T, R, InvoiceError]
                          ): F[Either[InvoiceError, T]] =
    monadF.map(cmd.run(input))(_.flatMap(decoder.decode))
}

class ProdApplicationWithProcessor(override val repo: InvoiceRepo[IO], override val ctaRepo: CtaRepo[IO]) extends InvoiceApplicationWithProcessor[Future, IO]

class TestApplicationWithProcessor(override val repo: InvoiceRepo[Id], override val ctaRepo: CtaRepo[Id]) extends InvoiceApplicationWithProcessor[Id, Id]
