package sample1.domain.entity

import cats.Monad

import scala.language.higherKinds

trait PersistenceRepo[F[_], ErrType, PersType, PersIdType] {
  def save(persistenceEntity: PersType)(implicit monad: Monad[F]): F[Either[ErrType, PersType]]

  def retrieve(id: PersIdType)(implicit monad: Monad[F]): F[Either[ErrType, PersType]]
}
