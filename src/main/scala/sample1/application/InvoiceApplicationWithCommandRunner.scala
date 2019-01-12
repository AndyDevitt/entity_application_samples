package sample1.application

import cats.effect.IO
import cats.{Id, Monad, ~>}
import sample1.domain.command._
import sample1.domain.cta.CtaRepo
import sample1.domain.{InvoiceError, InvoiceRepo}

import scala.concurrent.Future

trait InvoiceApplicationWithCommandRunner[F[_], G[_]] {

  def repo: InvoiceRepo[G]

  def ctaRepo: CtaRepo[G]

  final val input: DomainCommandInput[G] = new DomainCommandInput[G](repo, ctaRepo)

  def processCommand[R, T, C <: CommandG[G, DomainCommandInput[G], R, InvoiceError]](cmd: C)
                                                                                    (implicit monadF: Monad[F],
                                                                                     monadG: Monad[G],
                                                                                     naturalTransformation: G ~> F,
                                                                                     transformer: ApplicationTransformer[T, R, InvoiceError],
                                                                                     runner: CommandRunner[F, G, C, DomainCommandInput[G], R, InvoiceError]
                                                                                    ): F[Either[InvoiceError, T]] =
    monadF.map(runner.run(cmd, input))(_.flatMap(transformer.decode))
}

class ProdApplicationWithRunner(override val repo: InvoiceRepo[IO], override val ctaRepo: CtaRepo[IO]) extends InvoiceApplicationWithCommandRunner[Future, IO]

class TestApplicationWithRunner(override val repo: InvoiceRepo[Id], override val ctaRepo: CtaRepo[Id]) extends InvoiceApplicationWithCommandRunner[Id, Id]
