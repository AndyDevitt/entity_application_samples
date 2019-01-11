package sample1.domain.entity

trait VersionedEntity[EntType <: VersionedEntity[EntType, IdType], IdType <: EntityId] extends Entity[EntType, IdType] {
  def version: EntityVersion
}
