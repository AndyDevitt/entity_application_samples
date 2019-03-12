package sample1.application

import cats.{Monad, ~>}
import sample1.domain.command.{RunnableCommand, CommandInput}

trait EntityApplication[F[_], G[_], I <: CommandInput, Error] {

  def input: I

  /**
    * Process a generic command.
    *
    * @param cmd                   the command to be processed
    * @param monadF                monad instance for the application context type
    * @param monadG                monad instance for the underlying repository context type
    * @param naturalTransformation natural transformation from the repository context (G) to the application context (F)
    * @tparam R the successful result type of the command
    * @return returns the result of running the command
    */
  def processCommand[R](cmd: RunnableCommand[G, I, R, Error])
                       (implicit monadF: Monad[F],
                        monadG: Monad[G],
                        naturalTransformation: G ~> F
                       ): F[Either[Error, R]] =
    cmd.run(input)


  def processCommand[R, T](cmd: RunnableCommand[G, I, R, Error], transformer: R => Either[Error, T])
                          (implicit monadF: Monad[F],
                           monadG: Monad[G],
                           naturalTransformation: G ~> F
                          ): F[Either[Error, T]] =
    monadF.map(cmd.run(input))(_.flatMap(transformer))
}
