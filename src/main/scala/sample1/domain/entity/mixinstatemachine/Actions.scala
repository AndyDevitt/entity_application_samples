package sample1.domain.entity.mixinstatemachine

import sample1.domain.ActionStatus

trait Actions[EntType, ActionType] {
  def actionStatus(entity: EntType): Set[(ActionType, ActionStatus)]
}
