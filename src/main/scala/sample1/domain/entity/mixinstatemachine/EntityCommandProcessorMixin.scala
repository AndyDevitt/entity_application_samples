package sample1.domain.entity.mixinstatemachine

import sample1.domain.ActionStatus
import sample1.domain.entity.VersionedEntity

trait EntityCommandProcessorMixin[ActionType, ErrType, EntType <: VersionedEntity[_], PermissionType]
  extends Actions[EntType, ActionType, PermissionType] {

  import scala.language.implicitConversions

  override def actionStatus(permissions: PermissionType): Set[(ActionType, ActionStatus)] = Set.empty

  def notAllowedResult: Either[ErrType, EntType]

  // A very localised use of a helper implicit conversion so that Jacob wouldn't have to keep writing Right(...) :-)
  implicit def convertToSuccess(entity: EntType): Either[ErrType, EntType] = Right(entity)

  private def toActionStatus(checkResult: Either[ErrType, EntType]): ActionStatus =
    checkResult.fold[ActionStatus](e => ActionStatus.NotAllowedInCurrentState(e.toString), _ => ActionStatus.Allowed)

  protected def thisActionStatus(action: ActionType,
                                 entity: EntType,
                                 permissions: PermissionType,
                                 checkF: (EntType, PermissionType) => Either[ErrType, EntType]
                                ): Set[(ActionType, ActionStatus)] =
    Set((action, toActionStatus(checkF(entity, permissions))))
}
