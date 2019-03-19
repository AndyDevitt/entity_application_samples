package sample1.domain.command

final case class EntityResult[EntType, PermissionsType](entity: EntType, permissions: PermissionsType)
