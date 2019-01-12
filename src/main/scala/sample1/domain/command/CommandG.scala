package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain.entity._

/**
  * Base trait for the input to command when run. This will be extended to provide the runtime references to resources
  * owned by the application required for the command to be executed, for example repo or application service instance
  * references.
  */
trait CommandInput

/**
  * Generic command base trait which defines the highest level abstraction and the run method.
  *
  * @tparam F context in which the command is executed when run is called
  * @tparam I the input type for the command
  * @tparam R the success result type
  * @tparam E error type
  */
trait CommandG[F[_], -I <: CommandInput, R, E] extends Command {
  /**
    *
    * @param input     the command input that will contain any runtime references supplied by the application layer
    * @param monadF    monad instance for the context in which command is run (usually that of the repo)
    * @param transform natural transform instance to convert from the repo context to the application context
    * @tparam G the application context
    * @return returns the result of executing the command
    */
  def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[E, R]]
}

sealed trait CommandCategory

case object Update extends CommandCategory

case object Create extends CommandCategory

trait CommandX[C <: CommandCategory] extends Command {
  type F[_]
  type Res
  type Inp <: CommandInput
  type Err

  /**
    *
    * @param input     the command input that will contain any runtime references supplied by the application layer
    * @param monadF    monad instance for the context in which command is run (usually that of the repo)
    * @param transform natural transform instance to convert from the repo context to the application context
    * @tparam G the application context
    * @return returns the result of executing the command
    */
  def run[G[_]](input: Inp)(implicit monadF: Monad[F], transform: F ~> G): G[Either[Err, Res]]
}

object CommandG {
  type Aux[C <: CommandCategory, F0, Inp0, Res0, Err0] = CommandX[C] {
    type Res = Res0
    type Inp = Inp0
    type Err = Err0
  }
}

sealed trait EntityCommandG[F[_], -I <: CommandInput, R, E] extends CommandG[F, I, R, E] {

}

sealed trait OptimisticLockingG {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLockingG extends OptimisticLockingG {
  override def enforceOptimisticLocking: Boolean = false
}

trait EntityCreateCommandG[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[IdType]] extends EntityCommandG[F, I, EntType, E] {
  def action(): Either[E, EntType]

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[E, EntType]] =
    EntityRepoManager.manageCreate[G, F, I, EntityCreateCommandG[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)(() => this.action())
}

trait EntityRetrieveCommandG[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[IdType]] extends EntityCommandG[F, I, EntType, E] {
  def id: IdType

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[E, EntType]] =
    EntityRepoManager.manageRetrieve[G, F, I, EntityRetrieveCommandG[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)
}

trait EntityUpdateCommandG[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[IdType]] extends EntityCommandG[F, I, EntType, E] with OptimisticLockingG {
  def id: IdType

  def version: EntityVersion

  def action(entity: EntType): Either[E, EntType]

  def staleF(id: IdType): E

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[E, EntType]] =
    EntityRepoManager.manageUpdate[G, F, I, EntityUpdateCommandG[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)(this.action, staleF)
}
