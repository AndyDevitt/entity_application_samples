package sample1.domain.permissions

import sample1.domain.command.Command
import sample1.domain.entity.{Entity, EntityId}
import sample1.domain.user.UserId

trait Operable

trait Operation

trait Operator

trait PermissionsRetriever[F[_], PermissionsType]

trait CommandPermissionRetriever[F[_], PermissionsType, CmdType <: Command] {
  def retrieve(userId: UserId, cmd: CmdType): F[PermissionsType]
}

trait EntityPermissionsRetriever[F[_], IdType <: EntityId, EntityType <: Entity[IdType], PermissionsType, CmdType] extends PermissionsRetriever[F, PermissionsType] {
  def retrieve(userId: UserId, entity: EntityType, cmd: CmdType): F[PermissionsType]
}
