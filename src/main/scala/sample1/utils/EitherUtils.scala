package sample1.utils

object EitherUtils {

  def asRight[A, E](obj: A): Either[E, A] = Right(obj)

}
