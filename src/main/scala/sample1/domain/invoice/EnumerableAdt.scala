package sample1.domain.invoice

import shapeless.{:+:, CNil, Coproduct, Generic, Lazy, Witness}

trait EnumerableAdt[A] {
  def values: Set[A]
}

object EnumerableAdt {
  def apply[A](implicit enumerableAdt: EnumerableAdt[A]): Set[A] = enumerableAdt.values

  implicit def enumerable[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], allSingletons: Lazy[AllSingletons[A, C]]): EnumerableAdt[A] = new EnumerableAdt[A] {
    override def values: Set[A] = allSingletons.value.values.toSet
  }
}

trait AllSingletons[A, C <: Coproduct] {
  def values: List[A]
}

object AllSingletons {
  implicit def allSingletonsCons[A, H <: A, T <: Coproduct](implicit witness: Witness.Aux[H], allTs: Lazy[AllSingletons[A, T]]): AllSingletons[A, H :+: T] =
    new AllSingletons[A, H :+: T] {
      override def values: List[A] = witness.value :: allTs.value.values
    }

  implicit def allSingletonsCNil[A]: AllSingletons[A, CNil] = new AllSingletons[A, CNil] {
    override def values: List[A] = Nil
  }
}

object EnumerableTest extends App {

  import AllSingletons._

  sealed trait Action

  case object ApproveIt extends Action

  case object CommitIt extends Action


  val vals1: List[Action] = implicitly[AllSingletons[Action, ApproveIt.type :+: CommitIt.type :+: CNil]].values
  val vals2 = implicitly[EnumerableAdt[Action]].values
  val vals3 = EnumerableAdt[Action]
  //val vals3: Set[Action] = Values[Action]

  println(vals1)
  println(vals2)
  println(vals3)
  println(EnumerableAdt[InvoiceAction])
}
