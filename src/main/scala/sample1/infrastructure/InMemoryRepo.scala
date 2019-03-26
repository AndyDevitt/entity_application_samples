package sample1.infrastructure

import sample1.domain.entity._

trait InMemoryRepo[F[_], IdType <: EntityId, EntType <: VersionedEntity[IdType], ErrType]
  extends EntityRepoImpl[F, IdType, EntType, ErrType, IdType, EntType] {
  self =>

  def extractPersistenceKey(persEntity: EntType): IdType = persEntity.id

  def notFoundErrorF: IdType => ErrType

  def staleErrorF: IdType => ErrType

  override val persistenceRepo: PersistenceRepo[F, IdType, EntType, ErrType] = new InMemoryPersistenceRepo[F, IdType, EntType, ErrType] {
    override def notFoundErrorF: IdType => ErrType = self.notFoundErrorF

    override def staleErrorF: IdType => ErrType = self.staleErrorF
  }

}
