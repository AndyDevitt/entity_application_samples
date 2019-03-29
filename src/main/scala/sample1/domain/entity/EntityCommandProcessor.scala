package sample1.domain.entity

import cats.syntax.either._
import com.sun.tools.javac.code.TypeTag
import sample1.domain.command.{Command, EntityUpdateCommand}
import sample1.domain.{ActionStatus, NotAllowed}

trait EntityCommandProcessor[
F[_],
EntType <: VersionedEntity[_],
EntSubType <: EntType,
ErrType,
PermissionsType,
ActionBaseType,
ActionType <: ActionBaseType,
CmdType <: EntityUpdateCommand[F, _, ErrType, _, EntType, PermissionsType, ActionBaseType, ActionType],
ActionStatusType,
NotAllowedActionStatusType <: ActionStatusType] {

  import scala.language.implicitConversions

  def process(entity: EntType, cmd: CmdType, permissions: PermissionsType): Either[ErrType, EntType] =
    minimumAccessPermissionsCheck(entity, permissions)
      .flatMap(_ => canDo(entity, cmd.associatedAction, permissions))
      .left.map(statusToErrF)
      .flatMap(checkOptimisticLocking(_, cmd))
      .flatMap(action(_, cmd, permissions))

  def actionStatus(entity: EntType, action: ActionType, permissions: PermissionsType): ActionStatus =
    minimumAccessPermissionsCheck(entity, permissions)
      .flatMap(_ => canDo(entity, action, permissions))
      .fold[ActionStatus]((na: NotAllowed) => na.asInstanceOf[ActionStatus], _ => ActionStatus.Allowed)

  protected def minimumAccessPermissionsCheck(entity: EntType, permissions: PermissionsType): Either[NotAllowed, Unit]

  protected def canDo(entity: EntType, action: ActionType, permissions: PermissionsType): Either[NotAllowed, EntSubType]

  protected def statusToErrF: NotAllowed => ErrType

  protected def action(entity: EntSubType, cmd: CmdType, permissions: PermissionsType): Either[ErrType, EntType]

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
