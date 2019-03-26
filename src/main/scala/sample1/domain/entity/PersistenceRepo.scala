package sample1.domain.entity

import cats.Monad

trait PersistenceRepo[F[_], PersIdType, PersType, ErrType] {
  def save(persistenceEntity: PersType)(implicit monad: Monad[F]): F[Either[ErrType, PersType]]

  def retrieve(id: PersIdType)(implicit monad: Monad[F]): F[Either[ErrType, PersType]]
}
