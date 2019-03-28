package sample1.domain.entity

import cats.Monad

trait EntityRepo[F[_], IdType, EntType, ErrType] extends GenericRepo[F, ErrType] {
  def save(entity: EntType)(implicit monad: Monad[F]): F[Either[ErrType, EntType]]

  def retrieve(id: IdType)(implicit monad: Monad[F]): F[Either[ErrType, EntType]]
}
