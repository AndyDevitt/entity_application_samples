package sample1.application

import cats.Id
import cats.effect.IO
import sample1.domain.command.{CommandG, CommandRunner, DomainCommandInput}
import sample1.domain.{InvoiceError, InvoiceRepo}

import scala.concurrent.Future

trait InvoiceApplicationWithCommandRunner[F[_], G[_]] {

  def repo: InvoiceRepo[G]

  final val input: DomainCommandInput[G] = new DomainCommandInput[G](repo)

  def processCommand[C <: CommandG[G, DomainCommandInput[G], R, InvoiceError], R](cmd: C)
                                                                                 (implicit commandRunner: CommandRunner[G, F, C, DomainCommandInput[G], R, InvoiceError]
                                                                                 ): F[Either[InvoiceError, R]] =
    commandRunner.run(cmd, input)

  def processCommand2[R](cmd: CommandG[G, DomainCommandInput[G], R, InvoiceError])
                        (implicit commandRunner: CommandRunner[G, F, CommandG[G, DomainCommandInput[G], R, InvoiceError], DomainCommandInput[G], R, InvoiceError]
                        ): F[Either[InvoiceError, R]] =
    commandRunner.run(cmd, input)

}

class ProdApplicationWithRunner(override val repo: InvoiceRepo[IO]) extends InvoiceApplicationWithCommandRunner[Future, IO]

class TestApplicationWithRunner(override val repo: InvoiceRepo[Id]) extends InvoiceApplicationWithCommandRunner[Id, Id]
