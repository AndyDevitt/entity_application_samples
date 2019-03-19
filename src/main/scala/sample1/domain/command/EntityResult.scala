package sample1.domain.command

import sample1.domain.ActionStatus

final case class EntityResult[EntType, PermissionsType, ActionBaseType](entity: EntType,
                                                                        permissions: PermissionsType,
                                                                        actions: Set[(ActionBaseType, ActionStatus)])
