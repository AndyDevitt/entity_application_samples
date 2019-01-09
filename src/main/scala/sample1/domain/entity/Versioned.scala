package sample1.domain.entity

trait Versioned[A] {
  def incrementVersion(a: A): A
}

object Versioned {
  def instance[A](f: A => A): Versioned[A] = (a: A) => f(a)
}
