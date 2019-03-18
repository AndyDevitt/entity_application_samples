package sample1.domain.entity

import cats.syntax.either._
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
    canDo(entity, cmd.associatedAction, permissions)
      .left.map(statusToErrF)
      .flatMap(checkOptimisticLocking(_, cmd))
      .flatMap(actionWithFailure(_, cmd, permissions))

  def canDo(entity: EntType, action: ActionType, permissions: PermissionsType): Either[NotAllowedActionStatusType, EntSubType]

  def actionStatus(entity: EntType, action: ActionType, permissions: PermissionsType): ActionStatus =
    canDo(entity, action, permissions).fold[ActionStatus]((na: NotAllowedActionStatusType) => na.asInstanceOf[ActionStatus], _ => Allowed)

  protected def statusToErrF: NotAllowedActionStatusType => ErrType

  protected def action(entity: EntSubType, cmd: CmdType, permissions: PermissionsType): EntType

  protected def actionWithFailure(entity: EntSubType, cmd: CmdType, permissions: PermissionsType): Either[ErrType, EntType] =
    action(entity, cmd, permissions).asRight[ErrType]

  protected def staleF: EntType => ErrType

  private def checkOptimisticLocking(entity: EntSubType, cmd: Command): Either[ErrType, EntSubType] = cmd match {
    case c: EntityUpdateCommand[_, _, _, _, _, _, _] if c.enforceOptimisticLocking && c.version != entity.version => Left(staleF(entity))
    case _ => Right(entity)
  }
}
