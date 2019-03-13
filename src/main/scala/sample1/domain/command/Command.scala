package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain.entity._
import sample1.domain.user.UserId

/**
  * Base trait for all commands. Requires that a UserId be defined for all commands.
  */
trait Command {
  def userId: UserId
}

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
trait RunnableCommand[F[_], -I <: CommandInput, R, E] extends Command {
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

sealed trait EntityCommand[F[_], -I <: CommandInput, R, E] extends RunnableCommand[F, I, R, E]

sealed trait OptimisticLocking {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLocking extends OptimisticLocking {
  override def enforceOptimisticLocking: Boolean = false
}

trait EntityCreateCommand[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[IdType]] extends EntityCommand[F, I, EntType, E] {
  def create(): Either[E, EntType]

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[E, EntType]] =
    EntityRepoManager.manageCreate[G, F, I, EntityCreateCommand[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)
}

trait EntityRetrieveCommand[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[IdType]] extends EntityCommand[F, I, EntType, E] {
  def id: IdType

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[E, EntType]] =
    EntityRepoManager.manageRetrieve[G, F, I, EntityRetrieveCommand[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)
}

trait EntityUpdateCommand[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[IdType]] extends EntityCommand[F, I, EntType, E] with OptimisticLocking {
  def id: IdType

  def version: EntityVersion

  def action(entity: EntType): Either[E, EntType]

  def staleF(id: IdType): E

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[E, EntType]] =
    EntityRepoManager.manageUpdate[G, F, I, EntityUpdateCommand[F, I, E, IdType, EntType], IdType, EntType, E](extractRepo(input))(this)(staleF)
}

trait EntityQueryCommand[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[IdType], R, RepoType <: EntityRepo[F, IdType, EntType, E]] extends EntityCommand[F, I, R, E] {
  def extractRepo(input: I): RepoType

  def query(repo: RepoType): F[Either[E, R]]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[E, R]] =
    EntityRepoManager.manageQuery[G, F, I, EntityQueryCommand[F, I, E, IdType, EntType, R, RepoType], IdType, EntType, R, E, RepoType](extractRepo(input))(this)
}
