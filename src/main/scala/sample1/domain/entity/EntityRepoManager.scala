package sample1.domain.entity

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command._

object EntityRepoManager extends GenericRepoManager {

  def manageRetrieve[
  F[_],
  G[_],
  CmdType <: EntityRetrieveCommand[G]](cmd: CmdType)
                                      (repo: EntityRepo[G, cmd.Id, cmd.Entity, cmd.Error])
                                      (implicit monadG: Monad[G], transform: G ~> F
                                      ): F[Either[cmd.Error, EntityResult[cmd.Entity, cmd.Permissions, cmd.Actions]]] =
    transform((for {
      entity <- EitherT(repo.retrieve(cmd.id))
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId, entity))
      _ <- EitherT.fromEither(cmd.minimumAccessPermissionsCheck(entity, permissions))
      actions <- EitherT.pure[G, cmd.Error](cmd.extractActionStatuses(entity, permissions))
    } yield EntityResult(entity, permissions, actions)).value)

  def manageCreate[
  F[_],
  G[_],
  CmdType <: EntityCreateCommand[G]](cmd: CmdType)
                                    (repo: EntityRepo[G, cmd.Id, cmd.Entity, cmd.Error])
                                    (implicit monadG: Monad[G], transform: G ~> F
                                    ): F[Either[cmd.Error, EntityResult[cmd.Entity, cmd.Permissions, cmd.Actions]]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      created <- EitherT.fromEither[G](cmd.create(permissions))
      saved <- EitherT(repo.save(created))
      actions <- EitherT.pure[G, cmd.Error](cmd.extractActionStatuses(saved, permissions))
    } yield EntityResult(saved, permissions, actions)).value)

  def manageUpdate[
  F[_],
  G[_],
  CmdType <: EntityUpdateCommand[G]
  ](cmd: CmdType)
   (repo: EntityRepo[G, cmd.Id, cmd.Entity, cmd.Error])
   (staleF: cmd.Id => cmd.Error)
   (implicit monadG: Monad[G], transform: G ~> F
   ): F[Either[cmd.Error, EntityResult[cmd.Entity, cmd.Permissions, cmd.Actions]]] =
    transform((for {
      entity <- EitherT(repo.retrieve(cmd.id))
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId, entity))
      _ <- EitherT.fromEither(checkOptimisticLocking(entity, cmd, staleF))
      updated <- EitherT.fromEither[G](cmd.action(entity, permissions))
      saved <- EitherT(repo.save(updated))
      actions <- EitherT.pure[G, cmd.Error](cmd.extractActionStatuses(saved, permissions))
    } yield EntityResult(saved, permissions, actions)).value)

  private def checkOptimisticLocking[
  F[_],
  EntType <: VersionedEntity[IdType],
  IdType <: EntityId, E](entity: EntType, cmd: EntityUpdateCommand[F], staleF: IdType => E
                        ): Either[E, Unit] =
    Either.cond(cmd.version == entity.version, (), staleF(entity.id))

  def manageEntityQuery[
  F[_],
  G[_],
  CmdType <: EntityQueryCommand[G]](cmd: CmdType)
                                   (repo: cmd.Repo)
                                   (implicit monadG: Monad[G], transform: G ~> F
                                   ): F[Either[cmd.Error, List[EntityResult[cmd.Entity, cmd.Permissions, cmd.Actions]]]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      results <- EitherT(cmd.query(repo, permissions))
      res <- EitherT.pure[G, cmd.Error](results.map(ent => EntityResult(ent, permissions, cmd.extractActionStatuses(ent, permissions))))
    } yield res).value)

}
