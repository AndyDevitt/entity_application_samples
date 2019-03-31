package sample1.domain.entity

import sample1.domain.ActionStatus
import shapeless.{:+:, CNil, Coproduct, Generic, Witness}

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
