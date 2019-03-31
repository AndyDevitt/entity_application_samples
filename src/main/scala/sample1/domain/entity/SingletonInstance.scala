package sample1.domain.entity

import shapeless.Witness

trait SingletonInstance[A] {
  def instance(): A
}

object SingletonInstance {
  implicit def singletonInstance[A](implicit witness: Witness.Aux[A]): SingletonInstance[A] = () => witness.value
}
