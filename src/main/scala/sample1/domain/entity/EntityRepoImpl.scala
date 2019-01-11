package sample1.domain.entity

import cats.Monad
import sample1.domain.{Codec, Encoder}

trait EntityRepoImpl[F[_], IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType], ErrType, PersType, PersIdType] {

  def cleanEntityForResultAfterSaveF: EntType => EntType = (x: EntType) => x

  def persistenceRepo: PersistenceRepo[F, ErrType, PersType, PersIdType]

  def saveEntity(entity: EntType)
                (implicit monad: Monad[F], versioned: Versioned[EntType], codec: Codec[EntType, PersType, ErrType]
                ): F[Either[ErrType, EntType]] = {
    val newVer = versioned.incrementVersion(entity)
    monad.map(persistenceRepo.save(codec.encode(newVer)))(
      _.map(_ => cleanEntityForResultAfterSaveF(newVer)))
  }

  def retrieveEntity(id: IdType)
                    (implicit monad: Monad[F], codec: Codec[EntType, PersType, ErrType], idEncoder: Encoder[IdType, PersIdType]
                    ): F[Either[ErrType, EntType]] =
    monad.map(persistenceRepo.retrieve(idEncoder.encode(id)))(
      _.flatMap(codec.decode))
}
