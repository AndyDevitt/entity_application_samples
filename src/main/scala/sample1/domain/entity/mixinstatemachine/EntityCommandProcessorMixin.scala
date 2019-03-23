package sample1.domain.entity.mixinstatemachine

import sample1.domain.entity.VersionedEntity
import sample1.domain.{ActionStatus, Allowed, NotAllowedInCurrentState}

trait EntityCommandProcessorMixin[ActionType, ErrType, EntType <: VersionedEntity[_], PermissionType] extends Actions[EntType, ActionType] {

  import scala.language.implicitConversions

  override def actionStatus(): Set[(ActionType, ActionStatus)] = Set.empty

  def notAllowedResult: Either[ErrType, EntType]

  // A very localised use of a helper implicit conversion so that Jacob wouldn't have to keep writing Right(...) :-)
  implicit def convertToSuccess(entity: EntType): Either[ErrType, EntType] = Right(entity)

  private def toActionStatus(checkResult: Either[ErrType, EntType]): ActionStatus =
    checkResult.fold[ActionStatus](e => NotAllowedInCurrentState(e.toString), _ => Allowed)

  protected def thisActionStatus(action: ActionType, entity: EntType, checkF: EntType => Either[ErrType, EntType]
                                ): Set[(ActionType, ActionStatus)] =
    Set((action, toActionStatus(checkF(entity))))

  protected def thisActionStatus(action: ActionType
                                ): Set[(ActionType, ActionStatus)] =
    Set((action, Allowed))
}
