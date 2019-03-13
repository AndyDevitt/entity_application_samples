package sample1.utils

import sample1.utils.EitherUtils._

trait ReduceOptionWithFailure[A] {

  def reduceOptionWithFailure[E, B >: A](list: List[A], f: (B, A) => Either[E, B]): Either[E, Option[B]] =
    list.foldLeft(asRight[Option[B], E](Option.empty[B]))((b: Either[E, Option[B]], current) => b.flatMap {
      case Some(acc) => f(acc, current).map(Some(_))
      case None => Right(Some(current))
    })

}

object ReduceOptionWithFailure {

  implicit class ops[A](list: List[A]) {
    def reduceOptionWithFailure[E, B >: A](f: (B, A) => Either[E, B]): Either[E, Option[B]] =
      list.foldLeft(asRight[Option[B], E](Option.empty[B]))((b: Either[E, Option[B]], current) => b.flatMap {
        case Some(acc) => f(acc, current).map(Some(_))
        case None => Right(Some(current))
      })
  }

}
