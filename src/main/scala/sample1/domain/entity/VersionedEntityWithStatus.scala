package sample1.domain.entity

trait VersionedEntityWithStatus[IdType <: EntityId, StatusType] extends VersionedEntity[IdType] {
  def status: StatusType
}
