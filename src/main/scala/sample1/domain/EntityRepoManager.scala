package sample1.domain

import cats.data.EitherT
import cats.{Monad, ~>}
import sample1.domain.command._
import sample1.domain.entity.{EntityRepo, VersionedEntity}
import sample1.domain.invoice.EntityId

import scala.language.higherKinds

object EntityRepoManager {

  def manageCreate[F[_], G[_], I <: CommandInput[G], CmdType <: EntityCreateCommandG[G, I, E, IdType, EntType], IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType], R, E](cmd: CmdType)
                                                                                                                                                                                         (repo: EntityRepo[G, IdType, EntType, E])
                                                                                                                                                                                         (f: () => Either[E, EntType])
                                                                                                                                                                                         (implicit monadG: Monad[G], transform: G ~> F, decoder: Decoder[R, EntType, E]
                                                                                                                                                                                         ): F[Either[E, R]] =
    transform((for {
      updatedInv <- EitherT.fromEither[G](f())
      savedInv <- EitherT(repo.save(updatedInv))
      view <- EitherT.fromEither[G](decoder.decode(savedInv))
    } yield view).value)

  def manageUpdate[F[_], G[_], I <: CommandInput[G], CmdType <: EntityUpdateCommandG[G, I, E, IdType, EntType], IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType], R, E](repo: EntityRepo[G, IdType, EntType, E])
                                                                                                                                                                                      (cmd: CmdType)
                                                                                                                                                                                      (f: EntType => Either[E, EntType], staleF: IdType => E)
                                                                                                                                                                                      (implicit monadG: Monad[G], transform: G ~> F, decoder: Decoder[R, EntType, E]
                                                                                                                                                                                      ): F[Either[E, R]] =
    transform((for {
      inv <- EitherT(repo.retrieve(cmd.id))
      _ <- EitherT.fromEither(checkOptimisticLocking(inv, cmd, staleF))
      updatedInv <- EitherT.fromEither[G](f(inv))
      savedInv <- EitherT(repo.save(updatedInv))
      view <- EitherT.fromEither[G](decoder.decode(savedInv))
    } yield view).value)

  private def checkOptimisticLocking[F[_], EntType <: VersionedEntity[EntType, IdType], IdType <: EntityId, E](entity: EntType, cmd: EntityUpdateCommandG[F, _, _, _, _], staleF: IdType => E): Either[E, Unit] =
    Either.cond(cmd.version == entity.version, (), staleF(entity.id))

}
