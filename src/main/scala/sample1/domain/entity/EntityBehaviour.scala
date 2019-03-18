package sample1.domain.entity

import sample1.domain.command.{Command, EntityUpdateCommand}
import sample1.domain.{ActionStatus, Allowed}

trait EntityBehaviour[
F[_],
EntType <: VersionedEntity[_],
EntSubType <: EntType,
ErrType,
PermissionsType,
ActionType,
CmdType <: EntityUpdateCommand[F, _, ErrType, _, EntType, PermissionsType, ActionType],
ActionStatusType,
NotAllowedActionStatusType <: ActionStatusType] {
  def process(entity: EntType, cmd: CmdType, permissions: PermissionsType): Either[ErrType, EntType] =
    canDoWrapper(canDo(entity, cmd.associatedAction, permissions), cmd)
      .map(inv => action(inv, cmd, permissions))

  def processWithFailure(entity: EntType, cmd: CmdType, permissions: PermissionsType): Either[ErrType, EntType] =
    canDoWrapper(canDo(entity, cmd.associatedAction, permissions), cmd)
      .flatMap(inv => actionWithFailure(inv, cmd, permissions))

  def canDo(entity: EntType, action: ActionType, permissions: PermissionsType): Either[NotAllowedActionStatusType, EntType]

  def actionStatus(entity: EntType, action: ActionType, permissions: PermissionsType): ActionStatus =
    canDo(entity, action, permissions).fold[ActionStatus]((na: NotAllowedActionStatusType) => na.asInstanceOf[ActionStatus], _ => Allowed)

  protected def statusToErrF: NotAllowedActionStatusType => ErrType

  protected def action(entity: EntType, cmd: CmdType, permissions: PermissionsType): EntSubType

  protected def actionWithFailure(entity: EntType, cmd: CmdType, permissions: PermissionsType): Either[ErrType, EntSubType]

  protected def staleF: EntType => ErrType

  private def canDoWrapper(result: Either[NotAllowedActionStatusType, EntType], cmd: CmdType): Either[ErrType, EntType] =
    result
      .left.map(statusToErrF)
      .flatMap(inv => checkOptimisticLocking(inv, cmd))

  private def checkOptimisticLocking(entity: EntType, cmd: Command): Either[ErrType, EntType] = cmd match {
    case c: EntityUpdateCommand[_, _, _, _, _, _, _] if c.enforceOptimisticLocking && c.version != entity.version => Left(staleF(entity))
    case _ => Right(entity)
  }
}
