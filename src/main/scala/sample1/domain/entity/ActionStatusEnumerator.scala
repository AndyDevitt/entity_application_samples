package sample1.domain.entity

import sample1.domain.permissions.EntityPermissions
import sample1.domain.{ActionStatus, NotAllowed}
import shapeless.{:+:, CNil, Coproduct, Generic, Witness}

trait ActionAllowed[ActionType, EntityStatusType, EntType <: VersionedEntityWithStatus[_, EntityStatusType], EntSubType, EntityPermissionType, UserPermissionsType <: EntityPermissions[EntityPermissionType]] {

  private def commonChecks(entity: EntType, permissions: UserPermissionsType
                          ): Either[NotAllowed, EntSubType] =
    minimumAccessPermissionsCheck(entity, permissions)
      .flatMap(_ => checkRequiredPermissions(entity, permissions))
      .flatMap(_ => checkAllowedStatus(entity))
      .flatMap(_ => checkAdditionalRequirements(entity, permissions))

  def actionStatus(entity: EntType, permissions: UserPermissionsType): ActionStatus =
    commonChecks(entity, permissions)
      .fold[ActionStatus]((na: NotAllowed) => na.asInstanceOf[ActionStatus], _ => ActionStatus.Allowed)

  protected def minimumAccessPermissionsCheck(entity: EntType, permissions: UserPermissionsType): Either[NotAllowed, Unit]

  protected def checkAdditionalRequirements(entity: EntType,
                                            permissions: UserPermissionsType
                                           ): Either[NotAllowed, EntSubType] =
    downCaster(entity)

  protected def requiredPermissions: Set[EntityPermissionType]

  protected def allowedStatuses: Set[EntityStatusType]

  protected def checkRequiredPermissions(entity: EntType,
                                         permissions: UserPermissionsType
                                        ): Either[NotAllowed, Unit] =
    Either.cond(permissions.hasAll(requiredPermissions), (), ActionStatus.NotEnoughPermissions(
      s"User does not have sufficient permissions to perform the action, required: $requiredPermissions"))

  protected def checkAllowedStatus(entity: EntType): Either[NotAllowed, Unit] =
    Either.cond(allowedStatuses.contains(entity.status), (), ActionStatus.NotAllowedInCurrentStatus())

  protected def downCaster(entity: EntType): Either[NotAllowed, EntSubType]

  def actionAllowed(entity: EntType, permissions: UserPermissionsType): Either[NotAllowed, EntSubType]
}

object ActionAllowed {
  def apply[
  ActionType,
  EntityStatusType,
  EntType <: VersionedEntityWithStatus[_, EntityStatusType],
  EntSubType <: EntType,
  EntityPermissionType,
  UserPermissionsType <: EntityPermissions[EntityPermissionType]]
  (f: (EntType, UserPermissionsType) => Either[NotAllowed, EntSubType]
  ): ActionAllowed[ActionType, EntityStatusType, EntType, EntSubType, EntityPermissionType, UserPermissionsType] =
    new ActionAllowed[ActionType, EntityStatusType, EntType, EntSubType, EntityPermissionType, UserPermissionsType] {
      override protected def minimumAccessPermissionsCheck(entity: EntType, permissions: UserPermissionsType): Either[NotAllowed, Unit] = ???

      override protected def requiredPermissions: Set[EntityPermissionType] = ???

      override protected def allowedStatuses: Set[EntityStatusType] = ???

      override protected def downCaster(entity: EntType): Either[NotAllowed, EntSubType] = ???

      override def actionAllowed(entity: EntType, permissions: UserPermissionsType): Either[NotAllowed, EntSubType] = ???
    }
}

trait RetrieveActionStatus[Action, Entity, Permissions] {
  def actionStatus(action: Action, entity: Entity, permissions: Permissions): (Action, ActionStatus)
}

object RetrieveActionStatus {
  def apply[A, B, C](statusF: (A, B, C) => ActionStatus): RetrieveActionStatus[A, B, C] =
    (action: A, entity: B, permissions: C) => (action, statusF(action, entity, permissions))
}

trait AllSingletonsVisitor[A, C <: Coproduct, EntType, PermissionsType] {
  def values(entity: EntType, permissions: PermissionsType): List[(A, ActionStatus)]
}

object AllSingletonsVisitor {
  implicit def allSingletonsCons[A, H <: A, T <: Coproduct, EntType, PermissionsType](implicit
                                                                                      witness: Witness.Aux[H],
                                                                                      allTs: AllSingletonsVisitor[A, T, EntType, PermissionsType],
                                                                                      retrieveActionStatus: RetrieveActionStatus[H, EntType, PermissionsType]
                                                                                     ): AllSingletonsVisitor[A, H :+: T, EntType, PermissionsType] =
    (entity: EntType, permissions: PermissionsType) =>
      retrieveActionStatus.actionStatus(witness.value, entity, permissions) :: allTs.values(entity, permissions)

  implicit def allSingletonsCNil[A, EntType, PermissionsType]: AllSingletonsVisitor[A, CNil, EntType, PermissionsType] =
    (_: EntType, _: PermissionsType) => Nil
}

@scala.annotation.implicitNotFound("Please ensure there exists an implicit implementation of RetrieveActionStatus for each Action, Entity, and Permissions type")
trait ActionStatusEnumerator[A, EntType, PermissionsType] {
  def actionStatuses(entity: EntType, permissions: PermissionsType): Set[(A, ActionStatus)]
}

object ActionStatusEnumerator {
  def apply[A, EntType, PermissionsType](entity: EntType, permissions: PermissionsType)
                                        (implicit actionStatusEnumerator: ActionStatusEnumerator[A, EntType, PermissionsType]
                                        ): Set[(A, ActionStatus)] =
    actionStatusEnumerator.actionStatuses(entity, permissions)

  implicit def actionStatusEnumerable[A, EntType, PermissionsType, C <: Coproduct](implicit
                                                                                   gen: Generic.Aux[A, C],
                                                                                   allSingletonsVisitor: AllSingletonsVisitor[A, C, EntType, PermissionsType]
                                                                                  ): ActionStatusEnumerator[A, EntType, PermissionsType] =
    (entity: EntType, permissions: PermissionsType) => allSingletonsVisitor.values(entity, permissions).toSet
}
