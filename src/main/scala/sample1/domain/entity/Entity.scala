package sample1.domain.entity

import sample1.domain.invoice.EntityId

trait Entity[EntType <: Entity[EntType, IdType], IdType <: EntityId] {
  def id: IdType
}

final case class EntityVersion(version: Int) {
  def nextVersion: EntityVersion = copy(version = version + 1)

  def >(other: EntityVersion): Boolean = this.version > other.version

  def <(other: EntityVersion): Boolean = this.version < other.version
}

object EntityVersion {
  def apply(): EntityVersion = new EntityVersion(0)
}

trait VersionedEntity[EntType <: VersionedEntity[EntType, IdType], IdType <: EntityId] extends Entity[EntType, IdType] {
  def version: EntityVersion
}
