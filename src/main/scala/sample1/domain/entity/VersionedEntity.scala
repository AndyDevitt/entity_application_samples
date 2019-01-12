package sample1.domain.entity

trait VersionedEntity[IdType <: EntityId] extends Entity[IdType] {
  def version: EntityVersion
}
