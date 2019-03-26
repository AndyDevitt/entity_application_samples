package sample1.domain.entity

import cats.Monad
import sample1.domain.Encoder

/**
  * A generic implementation of an entity repository that manages saves with automatic version increments, retrieval,
  * and allows the injection of business transformation processes via the EntityRepoCodec instance. It requires a
  * PersistenceRepo to be injected that implements the actual storage, but manages the transformation between domain
  * and persistence types given the appropriate EntityRepoCodec.
  *
  * @tparam F          the context of the concrete repo implementation
  * @tparam IdType     the type of the entity identifier
  * @tparam EntType    the type of the entity
  * @tparam ErrType    error type
  * @tparam PersType   the type of the persistence object for the entity
  * @tparam PersIdType the type of the persistence object identifier
  */
trait EntityRepoImpl[F[_], IdType <: EntityId, EntType <: VersionedEntity[IdType], ErrType, PersType, PersIdType] {

  def persistenceRepo: PersistenceRepo[F, ErrType, PersType, PersIdType]

  def codec: EntityRepoCodec[EntType, PersType, ErrType]

  def saveEntity(entity: EntType)
                (implicit monad: Monad[F], versioned: Versioned[EntType]
                ): F[Either[ErrType, EntType]] = {
    val newVer = versioned.incrementVersion(entity)
    monad.map(persistenceRepo.save(codec.encode(newVer)))(
      _.map(_ => newVer))
  }

  def retrieveEntity(id: IdType)
                    (implicit monad: Monad[F], idEncoder: Encoder[IdType, PersIdType]
                    ): F[Either[ErrType, EntType]] =
    monad.map(persistenceRepo.retrieve(idEncoder.encode(id)))(
      _.flatMap(codec.decode))
}
