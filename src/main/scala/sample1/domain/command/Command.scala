package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain.entity._
import sample1.domain.permissions.{BasicPermissionRetriever, EntityPermissionsRetriever}
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
  * @tparam F       context in which the command is executed when run is called
  * @tparam I       the input type for the command
  * @tparam ResType the success result type
  * @tparam ErrType error type
  */
trait RunnableCommand[F[_], -I <: CommandInput, ResType, ErrType, PermissionsType] extends Command {
  /**
    *
    * @param input     the command input that will contain any runtime references supplied by the application layer
    * @param monadF    monad instance for the context in which command is run (usually that of the repo)
    * @param transform natural transform instance to convert from the repo context to the application context
    * @tparam G the application context
    * @return returns the result of executing the command
    */
  def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[ErrType, ResType]]
}

sealed trait EntityCommand[F[_], -I <: CommandInput, ResType, ErrType, PermissionsType]
  extends RunnableCommand[F, I, ResType, ErrType, PermissionsType]

sealed trait OptimisticLocking {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLocking extends OptimisticLocking {
  override def enforceOptimisticLocking: Boolean = false
}

trait EntityCreateCommand[F[_], -I <: CommandInput, ErrType, IdType <: EntityId, EntType <: VersionedEntity[IdType], PermissionsType]
  extends EntityCommand[F, I, EntType, ErrType, PermissionsType] {
  def create(permissions: PermissionsType): Either[ErrType, EntType]

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, ErrType]

  def permissionsRetriever: BasicPermissionRetriever[F, PermissionsType]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[ErrType, EntType]] =
    EntityRepoManager.manageCreate[G, F, I, EntityCreateCommand[F, I, ErrType, IdType, EntType, PermissionsType], IdType, EntType, ErrType, PermissionsType](extractRepo(input))(this)
}

trait EntityRetrieveCommand[F[_], -I <: CommandInput, ErrType, IdType <: EntityId, EntType <: VersionedEntity[IdType], PermissionsType] extends EntityCommand[F, I, EntType, ErrType, PermissionsType] {
  def id: IdType

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, ErrType]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[ErrType, EntType]] =
    EntityRepoManager.manageRetrieve[G, F, I, EntityRetrieveCommand[F, I, ErrType, IdType, EntType, PermissionsType], IdType, EntType, ErrType, PermissionsType](extractRepo(input))(this)
}

trait EntityUpdateCommand[
F[_],
-I <: CommandInput,
ErrType,
IdType <: EntityId,
EntType <: VersionedEntity[IdType],
PermissionsType]
  extends EntityCommand[F, I, EntType, ErrType, PermissionsType] with OptimisticLocking {
  def id: IdType

  def version: EntityVersion

  def action(entity: EntType, permissions: PermissionsType): Either[ErrType, EntType]

  def staleF(id: IdType): ErrType

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, ErrType]

  def permissionsRetriever: EntityPermissionsRetriever[F, IdType, EntType, PermissionsType]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[ErrType, EntType]] =
    EntityRepoManager.manageUpdate[G, F, I, IdType, EntType, ErrType, PermissionsType, EntityUpdateCommand[F, I, ErrType, IdType, EntType, PermissionsType]](extractRepo(input))(this)(staleF _)
}

trait EntityQueryCommand[
F[_],
-I <: CommandInput,
ErrType,
IdType <: EntityId,
EntType <: VersionedEntity[IdType],
ResType,
RepoType <: EntityRepo[F, IdType, EntType, ErrType],
PermissionsType]
  extends EntityCommand[F, I, ResType, ErrType, PermissionsType] {
  def extractRepo(input: I): RepoType

  def query(repo: RepoType): F[Either[ErrType, ResType]]

  override def run[G[_]](input: I)(implicit monadF: Monad[F], transform: F ~> G): G[Either[ErrType, ResType]] =
    EntityRepoManager.manageQuery[G, F, I, EntityQueryCommand[F, I, ErrType, IdType, EntType, ResType, RepoType, PermissionsType], IdType, EntType, ResType, ErrType, RepoType, PermissionsType](extractRepo(input))(this)
}
