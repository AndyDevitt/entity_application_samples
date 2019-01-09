package sample1.domain.entity

import cats.Monad
import sample1.domain.invoice.EntityId

import scala.language.higherKinds

trait EntityRepo[F[_], IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType], ErrType] {
  def save(entity: EntType)(implicit monad: Monad[F]): F[Either[ErrType, EntType]]

  def retrieve(id: IdType)(implicit monad: Monad[F]): F[Either[ErrType, EntType]]
}
