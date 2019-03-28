package sample1.infrastructure

import cats.Monad
import sample1.domain.entity.{EntityId, EntityVersion, PersistenceRepo, VersionedEntity}

import scala.collection.mutable

trait InMemoryPersistenceRepo[F[_], PersIdType <: EntityId, PersEntType <: VersionedEntity[PersIdType], ErrType]
  extends PersistenceRepo[F, PersIdType, PersEntType, ErrType] {

  def notFoundErrorF: PersIdType => ErrType

  def staleErrorF: PersIdType => ErrType

  override def save(persistenceEntity: PersEntType)(implicit monad: Monad[F]): F[Either[ErrType, Unit]] = {
    val key = extractPersistenceKey(persistenceEntity)
    monad.pure {
      if (store.get(key).isDefined) {
        Left(staleErrorF(key._1))
      } else {
        store(key) = persistenceEntity
        Right(())
      }
    }
  }

  override def retrieve(aId: PersIdType)(implicit monad: Monad[F]): F[Either[ErrType, PersEntType]] =
    monad.pure(store.filter(x => x._1._1 == aId)
      .reduceLeftOption(reduceF)
      .map(_._2)
      .toRight(notFoundErrorF(aId)))

  def currentStateStore(implicit monad: Monad[F]): F[Map[PersIdType, PersEntType]] =
    monad.pure(store
      .groupBy(groupByF)
      .mapValues(_.reduceLeft(reduceF)._2))

  def currentStateList(implicit monad: Monad[F]): F[List[PersEntType]] =
    monad.map(currentStateStore)(_.values.toList)

  private val store: mutable.Map[(PersIdType, EntityVersion), PersEntType] =
    mutable.Map()

  private def extractPersistenceKey(persEntity: PersEntType): (PersIdType, EntityVersion) =
    (persEntity.id, persEntity.version)

  private type KeyValuePair = ((PersIdType, EntityVersion), PersEntType)

  private val reduceF: (KeyValuePair, KeyValuePair) => KeyValuePair =
    (acc, next) => if (acc._1._2 > next._1._2) acc else next

  private val groupByF: KeyValuePair => PersIdType = x => x._1._1

}
