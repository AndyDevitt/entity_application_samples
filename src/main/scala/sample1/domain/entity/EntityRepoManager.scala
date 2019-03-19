package sample1.domain.entity

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command._

object EntityRepoManager {

  def manageRetrieve[
  F[_],
  G[_],
  InpType <: CommandInput,
  CmdType <: EntityRetrieveCommand[G, InpType, ErrType, IdType, EntType, PermissionsType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  ErrType,
  PermissionsType](repo: EntityRepo[G, IdType, EntType, ErrType])
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[ErrType, EntityResult[EntType, PermissionsType]]] =
    transform((for {
      entity <- EitherT(repo.retrieve(cmd.id))
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId, entity))
      _ <- EitherT.fromEither(cmd.checkMinimumPermissions(permissions))
    } yield EntityResult(entity, permissions)).value)

  def manageCreate[
  F[_],
  G[_],
  InpType <: CommandInput,
  CmdType <: EntityCreateCommand[G, InpType, ErrType, IdType, EntType, PermissionsType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  ErrType,
  PermissionsType](repo: EntityRepo[G, IdType, EntType, ErrType])
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[ErrType, EntityResult[EntType, PermissionsType]]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      _ <- EitherT.fromEither(cmd.checkMinimumPermissions(permissions))
      created <- EitherT.fromEither[G](cmd.create(permissions))
      saved <- EitherT(repo.save(created))
    } yield EntityResult(saved, permissions)).value)

  def manageUpdate[
  F[_],
  G[_],
  InpType <: CommandInput,
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  ErrType,
  PermissionsType,
  ActionType,
  CmdType <: EntityUpdateCommand[G, InpType, ErrType, IdType, EntType, PermissionsType, ActionType]
  ](repo: EntityRepo[G, IdType, EntType, ErrType])
   (cmd: CmdType)
   (staleF: IdType => ErrType)
   (implicit monadG: Monad[G], transform: G ~> F
   ): F[Either[ErrType, EntityResult[EntType, PermissionsType]]] =
    transform((for {
      entity <- EitherT(repo.retrieve(cmd.id))
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId, entity))
      _ <- EitherT.fromEither(cmd.checkMinimumPermissions(permissions))
      _ <- EitherT.fromEither(checkOptimisticLocking(entity, cmd, staleF))
      updated <- EitherT.fromEither[G](cmd.action(entity, permissions))
      saved <- EitherT(repo.save(updated))
    } yield EntityResult(saved, permissions)).value)

  private def checkOptimisticLocking[
  F[_],
  EntType <: VersionedEntity[IdType],
  IdType <: EntityId, E](entity: EntType, cmd: EntityUpdateCommand[F, _, _, _, _, _, _], staleF: IdType => E
                        ): Either[E, Unit] =
    Either.cond(cmd.version == entity.version, (), staleF(entity.id))

  def manageQuery[
  F[_],
  G[_],
  InpType <: CommandInput,
  CmdType <: EntityQueryCommand[G, InpType, ErrType, IdType, EntType, ResType, RepoType, PermissionsType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  ResType,
  ErrType,
  RepoType <: EntityRepo[G, IdType, EntType, ErrType],
  PermissionsType](repo: RepoType)
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[ErrType, ResType]] =
    transform((for {
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId))
      _ <- EitherT.fromEither(cmd.checkMinimumPermissions(permissions))
      results <- EitherT(cmd.query(repo))
    } yield results).value)

}
