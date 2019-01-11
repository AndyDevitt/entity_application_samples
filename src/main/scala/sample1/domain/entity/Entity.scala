package sample1.domain.entity

trait Entity[EntType <: Entity[EntType, IdType], IdType <: EntityId] {
  def id: IdType
}
