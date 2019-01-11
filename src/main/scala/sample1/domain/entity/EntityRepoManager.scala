package sample1.domain.entity

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command.{CommandInput, EntityCreateCommandG, EntityRetrieveCommandG, EntityUpdateCommandG}

object EntityRepoManager {

  def manageRetrieve[F[_], G[_], I <: CommandInput, CmdType <: EntityRetrieveCommandG[G, I, E, IdType, EntType], IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType], E](repo: EntityRepo[G, IdType, EntType, E])
                                                                                                                                                                                    (cmd: CmdType)
                                                                                                                                                                                    (implicit monadG: Monad[G], transform: G ~> F
                                                                                                                                                                                    ): F[Either[E, EntType]] =
    transform((for {
      inv <- EitherT(repo.retrieve(cmd.id))
    } yield inv).value)

  def manageCreate[F[_], G[_], I <: CommandInput, CmdType <: EntityCreateCommandG[G, I, E, IdType, EntType], IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType], E](repo: EntityRepo[G, IdType, EntType, E])
                                                                                                                                                                                (cmd: CmdType)
                                                                                                                                                                                (f: () => Either[E, EntType])
                                                                                                                                                                                (implicit monadG: Monad[G], transform: G ~> F
                                                                                                                                                                                ): F[Either[E, EntType]] =
    transform((for {
      updatedInv <- EitherT.fromEither[G](f())
      savedInv <- EitherT(repo.save(updatedInv))
    } yield savedInv).value)

  def manageUpdate[F[_], G[_], I <: CommandInput, CmdType <: EntityUpdateCommandG[G, I, E, IdType, EntType], IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType], E](repo: EntityRepo[G, IdType, EntType, E])
                                                                                                                                                                                (cmd: CmdType)
                                                                                                                                                                                (f: EntType => Either[E, EntType], staleF: IdType => E)
                                                                                                                                                                                (implicit monadG: Monad[G], transform: G ~> F
                                                                                                                                                                                ): F[Either[E, EntType]] =
    transform((for {
      inv <- EitherT(repo.retrieve(cmd.id))
      _ <- EitherT.fromEither(checkOptimisticLocking(inv, cmd, staleF))
      updatedInv <- EitherT.fromEither[G](f(inv))
      savedInv <- EitherT(repo.save(updatedInv))
    } yield savedInv).value)

  private def checkOptimisticLocking[F[_], EntType <: VersionedEntity[EntType, IdType], IdType <: EntityId, E](entity: EntType, cmd: EntityUpdateCommandG[F, _, _, _, _], staleF: IdType => E): Either[E, Unit] =
    Either.cond(cmd.version == entity.version, (), staleF(entity.id))

}
