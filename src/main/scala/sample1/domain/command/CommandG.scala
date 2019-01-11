package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain._
import sample1.domain.entity._
import sample1.domain.invoice._

trait CommandInput

trait CommandG[F[_], -I <: CommandInput, R, E] extends Command {
  def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, R]]
}

sealed trait EntityCommandG[F[_], -I <: CommandInput, R, E] extends CommandG[F, I, R, E] {

}

sealed trait OptimisticLockingG {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLockingG extends OptimisticLockingG {
  override def enforceOptimisticLocking: Boolean = false
}

trait EntityCreateCommandG[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType]] extends EntityCommandG[F, I, EntType, E] {
  def action(): Either[E, EntType]

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, EntType]] =
    EntityRepoManager.manageCreate[G, F, I, EntityCreateCommandG[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)(() => this.action())
}

trait EntityRetrieveCommandG[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType]] extends EntityCommandG[F, I, EntType, E] {
  def id: IdType

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, EntType]] =
    EntityRepoManager.manageRetrieve[G, F, I, EntityRetrieveCommandG[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)
}

trait EntityUpdateCommandG[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType]] extends EntityCommandG[F, I, EntType, E] with OptimisticLockingG {
  def id: IdType

  def version: EntityVersion

  def action(entity: EntType): Either[E, EntType]

  def staleF(id: IdType): E

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, EntType]] =
    EntityRepoManager.manageUpdate[G, F, I, EntityUpdateCommandG[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)(this.action, staleF)
}
