package sample1.application

import cats.{Monad, ~>}
import sample1.domain.command.{CommandInput, RunnableCommand}

trait EntityApplication[F[_], G[_], H[_], I <: CommandInput] {

  def input: I

  /**
    *
    * @param cmd                     the command to be processed
    * @param monadF                  monad instance for the application context type
    * @param monadG                  monad instance for the underlying repository context type
    * @param monadH                  monad instance for the permissions retriever context type
    * @param naturalTransformation   natural transformation from the repository context (G) to the application context (F)
    * @param naturalTransformationHG natural transformation from the permissions retriever context (H) to the repository context (G)
    * @tparam R               the successful result type of the command
    * @tparam Error           the error type
    * @tparam PermissionsType the permissions type for this command
    * @return
    */
  def processCommand[R, Error, PermissionsType](cmd: RunnableCommand[G, H, I, R, Error, PermissionsType])
                                               (implicit monadF: Monad[F],
                                                monadG: Monad[G],
                                                monadH: Monad[H],
                                                naturalTransformation: G ~> F,
                                                naturalTransformationHG: H ~> G
                                               ): F[Either[Error, R]] =
    cmd.run(input)


  def processCommand[R, T, Error, PermissionsType](cmd: RunnableCommand[G, H, I, R, Error, PermissionsType], transformer: R => Either[Error, T])
                                                  (implicit monadF: Monad[F],
                                                   monadG: Monad[G],
                                                   monadH: Monad[H],
                                                   naturalTransformation: G ~> F,
                                                   naturalTransformationHG: H ~> G
                                                  ): F[Either[Error, T]] =
    monadF.map(cmd.run(input))(_.flatMap(transformer))
}
