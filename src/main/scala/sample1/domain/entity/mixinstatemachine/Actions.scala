package sample1.domain.entity.mixinstatemachine

import sample1.domain.ActionStatus

trait Actions[EntType, ActionType, PermissionsType] {
  def actionStatus(permissions: PermissionsType): Set[(ActionType, ActionStatus)]
}
