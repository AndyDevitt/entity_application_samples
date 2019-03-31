package sample1.domain.entity

import sample1.domain.ActionStatus

trait EntityAlgebra[EntType <: Entity[_], ActionBaseType, PermissionsType, ErrType] {

  def minimumAccessPermissionsCheck(entity: EntType, permissions: PermissionsType
                                   ): Either[ErrType, Unit]

  def actionStatuses(entity: EntType,
                     permissions: PermissionsType)
                    (implicit
                     enumerator: ActionStatusEnumerator[ActionBaseType, EntType, PermissionsType]
                    ): Set[(ActionBaseType, ActionStatus)] =
    enumerator.actionStatuses(entity, permissions)

}
