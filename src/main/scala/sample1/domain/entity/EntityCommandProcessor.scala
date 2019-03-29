package sample1.domain.entity

import cats.syntax.either._
import sample1.domain.command.{Command, EntityUpdateCommand}
import sample1.domain.permissions.EntityPermissions
import sample1.domain.{ActionStatus, NotAllowed}

trait EntityCommandProcessor[
F[_],
EntType <: VersionedEntityWithStatus[_, EntityStatusType],
EntSubType <: EntType,
ErrType,
EntityPermissionType,
UserPermissionsType <: EntityPermissions[EntityPermissionType],
ActionBaseType,
ActionType <: ActionBaseType,
CmdType <: EntityUpdateCommand[F, _, ErrType, _, EntType, UserPermissionsType, ActionBaseType, ActionType],
ActionStatusType,
NotAllowedActionStatusType <: ActionStatusType,
EntityStatusType] {

  import scala.language.implicitConversions

  def process(entity: EntType, cmd: CmdType, permissions: UserPermissionsType): Either[ErrType, EntType] =
    commonChecks(entity, cmd.associatedAction, permissions)
      .left.map(statusToErrF)
      .flatMap(checkOptimisticLocking(_, cmd))
      .flatMap(action(_, cmd, permissions))

  private def commonChecks(entity: EntType, action: ActionType, permissions: UserPermissionsType): Either[NotAllowed, EntSubType] =
    minimumAccessPermissionsCheck(entity, permissions)
      .flatMap(_ => checkRequiredPermissions(entity, permissions))
      .flatMap(_ => checkAllowedStatus(entity))
      .flatMap(_ => canDo(entity, action, permissions))

  def actionStatus(entity: EntType, action: ActionType, permissions: UserPermissionsType): ActionStatus =
    commonChecks(entity, action, permissions)
      .fold[ActionStatus]((na: NotAllowed) => na.asInstanceOf[ActionStatus], _ => ActionStatus.Allowed)

  protected def minimumAccessPermissionsCheck(entity: EntType, permissions: UserPermissionsType): Either[NotAllowed, Unit]

  protected def canDo(entity: EntType, action: ActionType, permissions: UserPermissionsType): Either[NotAllowed, EntSubType] =
    downCaster(entity)

  protected def requiredPermissions: Set[EntityPermissionType]

  protected def allowedStatuses: Set[EntityStatusType]

  protected def checkRequiredPermissions(entity: EntType, permissions: UserPermissionsType): Either[NotAllowed, Unit] =
    Either.cond(permissions.hasAll(requiredPermissions), (), ActionStatus.NotEnoughPermissions(
      s"User does not have sufficient permissions to perform the action, required: $requiredPermissions"))

  protected def checkAllowedStatus(entity: EntType): Either[NotAllowed, Unit] =
    Either.cond(allowedStatuses.contains(entity.status), (), ActionStatus.NotEnoughPermissions(
      s"User does not have sufficient permissions to perform the action, required: $requiredPermissions"))

  protected def downCaster(entity: EntType): Either[NotAllowed, EntSubType]

  protected def statusToErrF: NotAllowed => ErrType

  protected def action(entity: EntSubType, cmd: CmdType, permissions: UserPermissionsType): Either[ErrType, EntType]

  // Localised implicit conversion to avoid having to type Right(...) for most state operations that always succeed
  protected implicit def convertEntityToSuccess(entity: EntType): Either[ErrType, EntType] = entity.asRight[ErrType]

  protected def staleF: EntType => ErrType

  private def checkOptimisticLocking(entity: EntSubType, cmd: Command): Either[ErrType, EntSubType] = cmd match {
    case c: EntityUpdateCommand[_, _, _, _, _, _, _, _] if c.enforceOptimisticLocking && c.version != entity.version =>
      staleF(entity).asLeft[EntSubType]
    case _ =>
      entity.asRight[ErrType]
  }
}
