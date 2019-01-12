package sample1.domain.entity

trait Entity[IdType <: EntityId] {
  def id: IdType
}
