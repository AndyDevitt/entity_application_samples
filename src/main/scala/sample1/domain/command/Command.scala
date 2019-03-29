package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain.ActionStatus
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
  * @tparam InpType the input type for the command
  * @tparam ResType the success result type
  * @tparam ErrType error type
  */
trait RunnableCommand[F[_], -InpType <: CommandInput, ResType, ErrType, PermissionsType] extends Command {
  /**
    *
    * @param input     the command input that will contain any runtime references supplied by the application layer
    * @param monadF    monad instance for the context in which command is run (usually that of the repo)
    * @param transform natural transform instance to convert from the repo context to the application context
    * @tparam G the application context
    * @return returns the result of executing the command
    */
  def run[G[_]](input: InpType)(implicit monadF: Monad[F], transform: F ~> G): G[Either[ErrType, ResType]]
}

sealed trait EntityCommand[
F[_],
-InpType <: CommandInput,
ResType,
ErrType,
IdType <: EntityId,
EntType <: VersionedEntity[IdType],
PermissionsType,
ActionsBaseType]
  extends RunnableCommand[F, InpType, ResType, ErrType, PermissionsType] {
  def extractActionStatuses(entity: EntType, permissions: PermissionsType): Set[(ActionsBaseType, ActionStatus)]
}

sealed trait OptimisticLocking {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLocking extends OptimisticLocking {
  override def enforceOptimisticLocking: Boolean = false
}

/**
  * Generic command for creating an entity. All commands that extend this command gain automatic management of the
  * following steps:
  *
  * 1) entity retrieval based on its ID
  * 2) permissions retrieval based on the retriever contained in the command
  * 3) calling the create method (which must be implemented in each concrete command)
  * 4) saving of the newly created entity
  * 5) returning the result which includes the entity, the retrieved permissions, and the status of all supported
  * actions
  *
  * @tparam F               context in which the command is executed when run is called
  * @tparam InpType         the input type for the command
  * @tparam ErrType         error type
  * @tparam IdType          entity ID type
  * @tparam EntType         entity type
  * @tparam PermissionsType permissions type for this entity / application
  * @tparam ActionsBaseType base type for the actions supported by the entity
  */
trait EntityCreateCommand[
F[_],
-InpType <: CommandInput,
ErrType,
IdType <: EntityId,
EntType <: VersionedEntity[IdType],
PermissionsType,
ActionsBaseType]
  extends EntityCommand[F, InpType, EntityResult[EntType, PermissionsType, ActionsBaseType], ErrType, IdType, EntType, PermissionsType, ActionsBaseType] {
  def create(permissions: PermissionsType): Either[ErrType, EntType]

  def extractRepo(input: InpType): EntityRepo[F, IdType, EntType, ErrType]

  def permissionsRetriever: BasicPermissionRetriever[F, PermissionsType]

  override def run[G[_]](input: InpType)
                        (implicit monadF: Monad[F], transform: F ~> G
                        ): G[Either[ErrType, EntityResult[EntType, PermissionsType, ActionsBaseType]]] =
    EntityRepoManager.manageCreate[
      G,
      F,
      InpType,
      EntityCreateCommand[F, InpType, ErrType, IdType, EntType, PermissionsType, ActionsBaseType],
      IdType,
      EntType,
      ErrType,
      PermissionsType,
      ActionsBaseType
      ](extractRepo(input))(this)
}

/**
  * Generic command for retrieving an entity. All commands that extend this command gain automatic management of the
  * following steps:
  *
  * 1) entity retrieval based on its ID
  * 2) permissions retrieval based on the retriever contained in the command
  * 3) calling the minimumAccessPermissionsCheck method (which must be implemented in each concrete command)
  * 4) returning the result which includes the entity, the retrieved permissions, and the status of all supported
  * actions
  *
  * @tparam F               context in which the command is executed when run is called
  * @tparam InpType         the input type for the command
  * @tparam ErrType         error type
  * @tparam IdType          entity ID type
  * @tparam EntType         entity type
  * @tparam PermissionsType permissions type for this entity / application
  * @tparam ActionsBaseType base type for the actions supported by the entity
  */
trait EntityRetrieveCommand[
F[_],
-InpType <: CommandInput,
ErrType,
IdType <: EntityId,
EntType <: VersionedEntity[IdType],
PermissionsType,
ActionsBaseType]
  extends EntityCommand[F, InpType, EntityResult[EntType, PermissionsType, ActionsBaseType], ErrType, IdType, EntType, PermissionsType, ActionsBaseType] {
  def id: IdType

  def extractRepo(input: InpType): EntityRepo[F, IdType, EntType, ErrType]

  def permissionsRetriever: EntityPermissionsRetriever[F, IdType, EntType, PermissionsType]

  def minimumAccessPermissionsCheck(entity: EntType, permissions: PermissionsType): Either[ErrType, Unit]

  override def run[G[_]](input: InpType)
                        (implicit monadF: Monad[F], transform: F ~> G
                        ): G[Either[ErrType, EntityResult[EntType, PermissionsType, ActionsBaseType]]] =
    EntityRepoManager.manageRetrieve[
      G,
      F,
      InpType,
      EntityRetrieveCommand[F, InpType, ErrType, IdType, EntType, PermissionsType, ActionsBaseType],
      IdType,
      EntType,
      ErrType,
      PermissionsType,
      ActionsBaseType
      ](extractRepo(input))(this)
}

/**
  * Generic command for updating an entity. All commands that extend this command gain automatic management of the
  * following steps:
  *
  * 1) entity retrieval based on its ID
  * 2) permissions retrieval based on the retriever contained in the command
  * 3) checking and enforcing the optimistic locking requirements of the command
  * 4) calling the action method (which must be implemented in each concrete command) supplying the retrieved
  * permissions
  * 5) saving of the updated entity
  * 6) returning the result which includes the entity, the retrieved permissions, and the status of all supported
  * actions
  *
  * @tparam F               context in which the command is executed when run is called
  * @tparam InpType         the input type for the command
  * @tparam ErrType         error type
  * @tparam IdType          entity ID type
  * @tparam EntType         entity type
  * @tparam PermissionsType permissions type for this entity / application
  * @tparam ActionsBaseType base type for the actions supported by the entity
  * @tparam ActionType      the specific action type that is supported by this command
  */
trait EntityUpdateCommand[
F[_],
-InpType <: CommandInput,
ErrType,
IdType <: EntityId,
EntType <: VersionedEntity[IdType],
PermissionsType,
ActionsBaseType,
ActionType <: ActionsBaseType]
  extends EntityCommand[F, InpType, EntityResult[EntType, PermissionsType, ActionsBaseType], ErrType, IdType, EntType, PermissionsType, ActionsBaseType]
    with OptimisticLocking {
  def id: IdType

  def version: EntityVersion

  def associatedAction: ActionType

  def action(entity: EntType, permissions: PermissionsType): Either[ErrType, EntType]

  def staleF(id: IdType): ErrType

  def extractRepo(input: InpType): EntityRepo[F, IdType, EntType, ErrType]

  def permissionsRetriever: EntityPermissionsRetriever[F, IdType, EntType, PermissionsType]

  override def run[G[_]](input: InpType)
                        (implicit monadF: Monad[F], transform: F ~> G
                        ): G[Either[ErrType, EntityResult[EntType, PermissionsType, ActionsBaseType]]] =
    EntityRepoManager.manageUpdate[
      G,
      F,
      InpType,
      IdType,
      EntType,
      ErrType,
      PermissionsType,
      ActionsBaseType,
      ActionType,
      EntityUpdateCommand[F, InpType, ErrType, IdType, EntType, PermissionsType, ActionsBaseType, ActionType]
      ](extractRepo(input))(this)(staleF)
}

/**
  * Generic command for retrieving a collection of complete entities. All commands that extend this command gain
  * automatic management of the following steps:
  *
  * 1) entity retrieval based on its ID
  * 2) permissions retrieval based on the retriever contained in the command
  * 3) calling the query method (which must be implemented in each concrete command) supplying the retrieved
  * permissions
  * 4) returning the retrieved collection of results, each of which includes the entity, the retrieved permissions, and
  * the status of all supported actions for the associated instance of the entity
  *
  * @tparam F               context in which the command is executed when run is called
  * @tparam InpType         the input type for the command
  * @tparam ErrType         error type
  * @tparam IdType          entity ID type
  * @tparam EntType         entity type
  * @tparam RepoType        the specific repo used in this query command
  * @tparam PermissionsType permissions type for this entity / application
  * @tparam ActionsBaseType base type for the actions supported by the entity
  */
trait EntityQueryCommand[
F[_],
-InpType <: CommandInput,
ErrType,
IdType <: EntityId,
EntType <: VersionedEntity[IdType],
RepoType <: EntityRepo[F, IdType, EntType, ErrType],
PermissionsType,
ActionsBaseType]
  extends EntityCommand[F, InpType, List[EntityResult[EntType, PermissionsType, ActionsBaseType]], ErrType, IdType, EntType, PermissionsType, ActionsBaseType] {
  def extractRepo(input: InpType): RepoType

  def query(repo: RepoType, permissions: PermissionsType): F[Either[ErrType, List[EntType]]]

  def permissionsRetriever: BasicPermissionRetriever[F, PermissionsType]

  override def run[G[_]](input: InpType)
                        (implicit monadF: Monad[F], transform: F ~> G
                        ): G[Either[ErrType, List[EntityResult[EntType, PermissionsType, ActionsBaseType]]]] =
    EntityRepoManager.manageEntityQuery[
      G,
      F,
      InpType,
      EntityQueryCommand[F, InpType, ErrType, IdType, EntType, RepoType, PermissionsType, ActionsBaseType],
      IdType,
      EntType,
      ErrType,
      RepoType,
      PermissionsType,
      ActionsBaseType
      ](extractRepo(input))(this)
}

/**
  * Generic command for performing a query against a repo interface. All commands that extend this command gain
  * automatic management of the following steps:
  *
  * 1) permissions retrieval based on the retriever contained in the command
  * 2) calling the query method (which must be implemented in each concrete command) supplying the retrieved
  * permissions
  * 3) returning the results of the query untransformed
  *
  * @tparam F               context in which the command is executed when run is called
  * @tparam InpType         the input type for the command
  * @tparam ErrType         error type
  * @tparam RepoType        the specific repo used in this query command
  * @tparam PermissionsType permissions type for this entity / application
  */
trait GenericQueryCommand[
F[_],
-InpType <: CommandInput,
ErrType,
ResType,
RepoType <: GenericRepo[F, ErrType],
PermissionsType]
  extends RunnableCommand[F, InpType, ResType, ErrType, PermissionsType] {
  def extractRepo(input: InpType): RepoType

  def query(repo: RepoType, permissions: PermissionsType): F[Either[ErrType, ResType]]

  def permissionsRetriever: BasicPermissionRetriever[F, PermissionsType]

  override def run[G[_]](input: InpType)(implicit monadF: Monad[F], transform: F ~> G): G[Either[ErrType, ResType]] =
    EntityRepoManager.manageGenericQuery[
      G,
      F,
      InpType,
      GenericQueryCommand[F, InpType, ErrType, ResType, RepoType, PermissionsType],
      ResType,
      ErrType,
      RepoType,
      PermissionsType
      ](extractRepo(input))(this)
}

/**
  * Generic command for calling any kind of domain service. All commands that extend this command gain automatic
  * management of the following steps:
  *
  * 1) permissions retrieval based on the retriever contained in the command
  * 2) calling the action method (which must be implemented in each concrete command) supplying the retrieved
  * permissions
  * 3) returning the results of the domain service
  *
  * The domain service is therefore responsible for any interaction with the repository or any other services available
  * from the command input. In essence, this allows a generic command to be supported in the same application interface
  * with permission retrieval.
  *
  * @tparam F               context in which the command is executed when run is called
  * @tparam InpType         the input type for the command
  * @tparam ErrType         error type
  * @tparam ResType         the success result type
  * @tparam RepoType        the specific repo used in this query command
  * @tparam PermissionsType permissions type for this entity / application
  */
trait DomainServiceCommand[
F[_],
-InpType <: CommandInput,
ErrType,
ResType,
RepoType <: GenericRepo[F, ErrType],
PermissionsType]
  extends RunnableCommand[F, InpType, ResType, ErrType, PermissionsType] {
  def extractRepo(input: InpType): RepoType

  def action(input: InpType, permissions: PermissionsType)(implicit monadF: Monad[F]): F[Either[ErrType, ResType]]

  def permissionsRetriever: BasicPermissionRetriever[F, PermissionsType]

  override def run[G[_]](input: InpType)(implicit monadF: Monad[F], transform: F ~> G): G[Either[ErrType, ResType]] =
    EntityRepoManager.manageDomainService[
      G,
      F,
      InpType,
      DomainServiceCommand[F, InpType, ErrType, ResType, RepoType, PermissionsType],
      ResType,
      ErrType,
      RepoType,
      PermissionsType
      ](input)(this)
}
