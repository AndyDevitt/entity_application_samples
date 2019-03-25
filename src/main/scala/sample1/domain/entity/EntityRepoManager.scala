package sample1.domain.entity

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command._

object EntityRepoManager {

  def manageRetrieve[
  F[_],
  G[_],
  InpType <: CommandInput,
  CmdType <: EntityRetrieveCommand[G, InpType, ErrType, IdType, EntType, PermissionsType, ActionsBaseType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  ErrType,
  PermissionsType,
  ActionsBaseType](repo: EntityRepo[G, IdType, EntType, ErrType])
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[ErrType, EntityResult[EntType, PermissionsType, ActionsBaseType]]] =
    transform((for {
      entity <- EitherT(repo.retrieve(cmd.id))
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId, entity))
      _ <- EitherT.fromEither(cmd.minimumAccessPermissionsCheck(entity, permissions))
      actions <- EitherT.pure[G, ErrType](cmd.extractActionStatuses(entity, permissions))
    } yield EntityResult(entity, permissions, actions)).value)

  def manageCreate[
  F[_],
  G[_],
  InpType <: CommandInput,
  CmdType <: EntityCreateCommand[G, InpType, ErrType, IdType, EntType, PermissionsType, ActionsBaseType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  ErrType,
  PermissionsType,
  ActionsBaseType](repo: EntityRepo[G, IdType, EntType, ErrType])
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[ErrType, EntityResult[EntType, PermissionsType, ActionsBaseType]]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      created <- EitherT.fromEither[G](cmd.create(permissions))
      saved <- EitherT(repo.save(created))
      actions <- EitherT.pure[G, ErrType](cmd.extractActionStatuses(saved, permissions))
    } yield EntityResult(saved, permissions, actions)).value)

  def manageUpdate[
  F[_],
  G[_],
  InpType <: CommandInput,
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  ErrType,
  PermissionsType,
  ActionsBaseType,
  ActionType <: ActionsBaseType,
  CmdType <: EntityUpdateCommand[G, InpType, ErrType, IdType, EntType, PermissionsType, ActionsBaseType, ActionType]
  ](repo: EntityRepo[G, IdType, EntType, ErrType])
   (cmd: CmdType)
   (staleF: IdType => ErrType)
   (implicit monadG: Monad[G], transform: G ~> F
   ): F[Either[ErrType, EntityResult[EntType, PermissionsType, ActionsBaseType]]] =
    transform((for {
      entity <- EitherT(repo.retrieve(cmd.id))
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId, entity))
      _ <- EitherT.fromEither(checkOptimisticLocking(entity, cmd, staleF))
      updated <- EitherT.fromEither[G](cmd.action(entity, permissions))
      saved <- EitherT(repo.save(updated))
      actions <- EitherT.pure[G, ErrType](cmd.extractActionStatuses(saved, permissions))
    } yield EntityResult(saved, permissions, actions)).value)

  private def checkOptimisticLocking[
  F[_],
  EntType <: VersionedEntity[IdType],
  IdType <: EntityId, E](entity: EntType, cmd: EntityUpdateCommand[F, _, _, _, _, _, _, _], staleF: IdType => E
                        ): Either[E, Unit] =
    Either.cond(cmd.version == entity.version, (), staleF(entity.id))

  def manageQuery[
  F[_],
  G[_],
  InpType <: CommandInput,
  CmdType <: EntityQueryCommand[G, InpType, ErrType, IdType, EntType, ResType, RepoType, PermissionsType, ActionsBaseType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  ResType,
  ErrType,
  RepoType <: EntityRepo[G, IdType, EntType, ErrType],
  PermissionsType,
  ActionsBaseType](repo: RepoType)
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[ErrType, ResType]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      results <- EitherT(cmd.query(repo, permissions))
    } yield results).value)

}
