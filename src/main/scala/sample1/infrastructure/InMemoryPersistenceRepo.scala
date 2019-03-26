package sample1.infrastructure

import cats.Monad
import sample1.domain.entity.{EntityId, EntityVersion, PersistenceRepo, VersionedEntity}

import scala.collection.mutable

trait InMemoryPersistenceRepo[F[_], ErrType, PersEntType <: VersionedEntity[PersIdType], PersIdType <: EntityId]
  extends PersistenceRepo[F, ErrType, PersEntType, PersIdType] {

  def notFoundErrorF: PersIdType => ErrType

  def staleErrorF: PersIdType => ErrType

  override def save(persistenceEntity: PersEntType)(implicit monad: Monad[F]): F[Either[ErrType, PersEntType]] = {
    val key = extractPersistenceKey(persistenceEntity)
    monad.pure {
      if (store.get(key).isDefined) {
        Left(staleErrorF(key._1))
      } else {
        store(key) = persistenceEntity
        Right(persistenceEntity)
      }
    }
  }

  override def retrieve(aId: PersIdType)(implicit monad: Monad[F]): F[Either[ErrType, PersEntType]] =
    monad.pure(store.filter(x => x._1._1 == aId)
      .reduceLeftOption(reduceF)
      .map(_._2)
      .toRight(notFoundErrorF(aId)))

  private val store: mutable.Map[(PersIdType, EntityVersion), PersEntType] =
    mutable.Map()

  private def extractPersistenceKey(persEntity: PersEntType): (PersIdType, EntityVersion) =
    (persEntity.id, persEntity.version)

  private type KeyValuePair = ((PersIdType, EntityVersion), PersEntType)

  private val reduceF: (KeyValuePair, KeyValuePair) => KeyValuePair =
    (acc, next) => if (acc._1._2 > next._1._2) acc else next

}
