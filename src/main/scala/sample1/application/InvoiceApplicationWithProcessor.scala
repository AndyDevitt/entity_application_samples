package sample1.application

import cats.effect.IO
import cats.{Id, Monad, ~>}
import sample1.domain.command._
import sample1.domain.cta.CtaRepo
import sample1.domain.{InvoiceError, InvoiceRepo}

import scala.concurrent.Future

trait InvoiceApplicationWithProcessor[F[_], G[_]] {

  def repo: InvoiceRepo[G]

  def ctaRepo: CtaRepo[G]

  final val input: DomainCommandInput[G] = new DomainCommandInput[G](repo, ctaRepo)

  /**
    * Process a generic command.
    *
    * @param cmd                   the command to be processed
    * @param monadF                monad instance for the underlying repository context type
    * @param monadG                monad instance for the application context type
    * @param naturalTransformation natural transformation from the repository context (F) to the application context (G)
    * @param transformer           transformer instance to convert from the command result type to the application result type
    * @tparam R the successful result type of the command
    * @tparam T the required sucess result type for the caller
    * @return returns the result of running the command, transformed into the callers desired type
    */
  def processCommand[R](cmd: CommandG[G, DomainCommandInput[G], R, InvoiceError])
                       (implicit monadF: Monad[F],
                        monadG: Monad[G],
                        naturalTransformation: G ~> F
                       ): F[Either[InvoiceError, R]] =
    cmd.run(input)
}

class ProdApplicationWithProcessor(override val repo: InvoiceRepo[IO], override val ctaRepo: CtaRepo[IO]) extends InvoiceApplicationWithProcessor[Future, IO]

class TestApplicationWithProcessor(override val repo: InvoiceRepo[Id], override val ctaRepo: CtaRepo[Id]) extends InvoiceApplicationWithProcessor[Id, Id]
