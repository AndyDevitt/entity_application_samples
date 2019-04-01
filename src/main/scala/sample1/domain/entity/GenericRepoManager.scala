package sample1.domain.entity

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command.{DomainServiceCommand, GenericQueryCommand}

trait GenericRepoManager {

  def manageGenericQuery[
  F[_],
  G[_],
  CmdType <: GenericQueryCommand[G]](cmd: CmdType)
                                    (repo: cmd.Repo)
                                    (implicit monadG: Monad[G], transform: G ~> F
                                    ): F[Either[cmd.Error, cmd.Result]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      results <- EitherT(cmd.query(repo, permissions))
    } yield results).value)

  def manageDomainService[
  F[_],
  G[_],
  CmdType <: DomainServiceCommand[G]](cmd: CmdType)
                                     (input: cmd.Input)
                                     (implicit monadG: Monad[G], transform: G ~> F
                                     ): F[Either[cmd.Error, cmd.Result]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      result <- EitherT(cmd.action(input, permissions))
    } yield result).value)

}
