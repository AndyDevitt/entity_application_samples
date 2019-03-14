package sample1.domain.entity

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command._

object EntityRepoManager {

  def manageRetrieve[
  F[_],
  G[_],
  I <: CommandInput,
  CmdType <: EntityRetrieveCommand[G, I, E, IdType, EntType, PermissionsType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  E,
  PermissionsType](repo: EntityRepo[G, IdType, EntType, E])
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[E, EntType]] =
    transform((for {
      inv <- EitherT(repo.retrieve(cmd.id))
    } yield inv).value)

  def manageCreate[
  F[_],
  G[_],
  I <: CommandInput,
  CmdType <: EntityCreateCommand[G, I, E, IdType, EntType, PermissionsType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  E,
  PermissionsType](repo: EntityRepo[G, IdType, EntType, E])
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[E, EntType]] =
    transform((for {
      updatedInv <- EitherT.fromEither[G](cmd.create())
      savedInv <- EitherT(repo.save(updatedInv))
    } yield savedInv).value)

  def manageUpdate[
  F[_],
  G[_],
  I <: CommandInput,
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  E,
  PermissionsType,
  CmdType <: EntityUpdateCommand[G, I, E, IdType, EntType, PermissionsType]
  ](repo: EntityRepo[G, IdType, EntType, E])
   (cmd: CmdType)
   (staleF: IdType => E)
   (implicit monadG: Monad[G], transform: G ~> F
   ): F[Either[E, EntType]] =
    transform((for {
      inv <- EitherT(repo.retrieve(cmd.id))
      _ <- EitherT.fromEither(checkOptimisticLocking(inv, cmd, staleF))
      permissions <- EitherT.right(cmd.permissionsRetriever.retrieve(cmd.userId, inv))
      updatedInv <- EitherT.fromEither[G](cmd.action(inv, permissions))
      savedInv <- EitherT(repo.save(updatedInv))
    } yield savedInv).value)

  private def checkOptimisticLocking[
  F[_],
  EntType <: VersionedEntity[IdType],
  IdType <: EntityId, E](entity: EntType, cmd: EntityUpdateCommand[F, _, _, _, _, _], staleF: IdType => E
                        ): Either[E, Unit] =
    Either.cond(cmd.version == entity.version, (), staleF(entity.id))

  def manageQuery[
  F[_],
  G[_],
  I <: CommandInput,
  CmdType <: EntityQueryCommand[G, I, E, IdType, EntType, R, RepoType, PermissionsType],
  IdType <: EntityId,
  EntType <: VersionedEntity[IdType],
  R,
  E,
  RepoType <: EntityRepo[G, IdType, EntType, E],
  PermissionsType](repo: RepoType)
                  (cmd: CmdType)
                  (implicit monadG: Monad[G], transform: G ~> F
                  ): F[Either[E, R]] =
    transform((for {
      results <- EitherT(cmd.query(repo))
    } yield results).value)

}
