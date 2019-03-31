package sample1.domain.invoice

import sample1.domain.ActionStatus
import shapeless.{:+:, CNil, Coproduct, Generic, Witness}

/**
  * Type class to enumerate all types in an ADT for any ADT that contains only singletons (i.e. all case objects).
  *
  * @tparam A The sealed trait parent for which the sub-class hierarchy instances should be deduced.
  */
trait EnumerableAdt[A] {
  def values: Set[A]
}

object EnumerableAdt {
  def apply[A](implicit enumerableAdt: EnumerableAdt[A]): Set[A] = enumerableAdt.values

  implicit def enumerable[A, C <: Coproduct](implicit gen: Generic.Aux[A, C], allSingletons: AllSingletons[A, C]): EnumerableAdt[A] = new EnumerableAdt[A] {
    override def values: Set[A] = allSingletons.values.toSet
  }
}

trait AllSingletons[A, C <: Coproduct] {
  def values: List[A]
}

object AllSingletons {
  implicit def allSingletonsCons[A, H <: A, T <: Coproduct](implicit witness: Witness.Aux[H], allTs: AllSingletons[A, T]): AllSingletons[A, H :+: T] =
    new AllSingletons[A, H :+: T] {
      override def values: List[A] = witness.value :: allTs.values
    }

  implicit def allSingletonsCNil[A]: AllSingletons[A, CNil] = new AllSingletons[A, CNil] {
    override def values: List[A] = Nil
  }
}

@scala.annotation.implicitNotFound("Please ensure there exists an implicit implementation of RetrieveActionStatus for each Action, Entity, and Permissions type")
trait RetrieveActionStatus[Action, Entity, Permissions] {
  def actionStatus(action: Action, entity: Entity, permissions: Permissions): (Action, ActionStatus)
}

object RetrieveActionStatus {
  def apply[A, B, C](statusF: (A, B, C) => ActionStatus): RetrieveActionStatus[A, B, C] =
    (action: A, entity: B, permissions: C) => (action, statusF(action, entity, permissions))
}

trait AllSingletonsVisitor[A, C <: Coproduct, EntType, PermissionsType] {
  def values(entity: EntType, permissions: PermissionsType): List[(A, ActionStatus)]
}

object AllSingletonsVisitor {
  implicit def allSingletonsCons[A, H <: A, T <: Coproduct, EntType, PermissionsType](implicit
                                                                                      witness: Witness.Aux[H],
                                                                                      allTs: AllSingletonsVisitor[A, T, EntType, PermissionsType],
                                                                                      retrieveActionStatus: RetrieveActionStatus[H, EntType, PermissionsType]
                                                                                     ): AllSingletonsVisitor[A, H :+: T, EntType, PermissionsType] =
    new AllSingletonsVisitor[A, H :+: T, EntType, PermissionsType] {
      override def values(entity: EntType, permissions: PermissionsType): List[(A, ActionStatus)] =
        retrieveActionStatus.actionStatus(witness.value, entity, permissions) :: allTs.values(entity, permissions)
    }

  implicit def allSingletonsCNil[A, EntType, PermissionsType]: AllSingletonsVisitor[A, CNil, EntType, PermissionsType] =
    new AllSingletonsVisitor[A, CNil, EntType, PermissionsType] {
      override def values(entity: EntType, permissions: PermissionsType): List[(A, ActionStatus)] = Nil
    }
}

@scala.annotation.implicitNotFound("Please ensure there exists an implicit implementation of RetrieveActionStatus for each Action, Entity, and Permissions type")
trait ActionStatusEnumerator[A, EntType, PermissionsType] {
  def actionStatuses(entity: EntType, permissions: PermissionsType): Set[(A, ActionStatus)]
}

object ActionStatusEnumerator {
  def apply[A, EntType, PermissionsType](entity: EntType, permissions: PermissionsType)
                                        (implicit actionStatusEnumerator: ActionStatusEnumerator[A, EntType, PermissionsType]
                                        ): Set[(A, ActionStatus)] =
    actionStatusEnumerator.actionStatuses(entity, permissions)

  implicit def actionStatusEnumerable[A, EntType, PermissionsType, C <: Coproduct]
  (implicit
   gen: Generic.Aux[A, C],
   allSingletonsVisitor: AllSingletonsVisitor[A, C, EntType, PermissionsType]
  ): ActionStatusEnumerator[A, EntType, PermissionsType] =
    new ActionStatusEnumerator[A, EntType, PermissionsType] {
      override def actionStatuses(entity: EntType, permissions: PermissionsType): Set[(A, ActionStatus)] =
        allSingletonsVisitor.values(entity, permissions).toSet
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
