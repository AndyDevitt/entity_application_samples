package sample1.domain.entity

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command.{CommandInput, GenericQueryCommand}

trait GenericRepoManager {

  def manageGenericQuery[
  F[_],
  G[_],
  InpType <: CommandInput,
  CmdType <: GenericQueryCommand[G, InpType, ErrType, ResType, RepoType, PermissionsType],
  ResType,
  ErrType,
  RepoType <: GenericRepo[G, ErrType],
  PermissionsType](repo: RepoType)
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[ErrType, ResType]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      results <- EitherT(cmd.query(repo, permissions))
    } yield results).value)

}
