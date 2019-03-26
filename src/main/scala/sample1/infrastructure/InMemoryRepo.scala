package sample1.infrastructure

import sample1.domain.entity._

trait InMemoryRepo[F[_], IdType <: EntityId, EntType <: VersionedEntity[IdType], ErrType]
  extends EntityRepoImpl[F, IdType, EntType, ErrType, EntType, IdType] {
  self =>

  def extractPersistenceKey(persEntity: EntType): IdType = persEntity.id

  def notFoundErrorF: IdType => ErrType

  def staleErrorF: IdType => ErrType

  override val persistenceRepo: PersistenceRepo[F, ErrType, EntType, IdType] = new InMemoryPersistenceRepo[F, ErrType, EntType, IdType] {
    override def notFoundErrorF: IdType => ErrType = self.notFoundErrorF

    override def staleErrorF: IdType => ErrType = self.staleErrorF
  }

}
