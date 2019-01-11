package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain._
import sample1.domain.entity._
import sample1.domain.invoice._

import scala.language.higherKinds

trait CommandInput[F[_]] {
  //def repo: InvoiceRepo[F]
}

class DomainCommandInput[F[_]](val repo: EntityRepo[F,InvoiceId,Invoice,InvoiceError]) extends CommandInput[F]

trait CommandRunner[F[_], G[_], C <: CommandG[F, I, R, E], -I <: CommandInput[F], R, E] {
  def run(command: C, input: I): G[Either[E, R]]
}

object CommandRunner {

  implicit def invoiceUpdateCommandRunner[F[_], G[_]](implicit transformer: F ~> G, monadF: Monad[F]
                                                     ): CommandRunner[F, G, InvoiceUpdateCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] =
    ??? //(command: InvoiceUpdateCommandG[F], input: DomainCommandInput[F]) => InvoiceRepoManager.manageUpdate(input.repo)(command)(command.action)

  //  implicit val idInvoiceUpdateCommandRunner = invoiceUpdateCommandRunner[Id,Id]
}


trait CommandG[F[_], -I <: CommandInput[F], R, E] extends Command {
  def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, R]]
}

//object CommandG {
//  type Aux[C0, R0] = CommandG[_, _, R0, _]
//}

sealed trait EntityCommandG[F[_], -I <: CommandInput[F], R, E] extends CommandG[F, I, R, E]

sealed trait OptimisticLockingG {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLockingG extends OptimisticLockingG {
  override def enforceOptimisticLocking: Boolean = false
}

sealed trait EntityCreateCommandG[F[_], -I <: CommandInput[F], E, IdType<:EntityId, EntType<:VersionedEntity[EntType,IdType]] extends EntityCommandG[F, I, EntType, E] {
  def action(): Either[E, EntType]

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, EntType]] =
    EntityRepoManager.manageCreate[G, F, I, EntityCreateCommandG[F,I,E,IdType,EntType], IdType, EntType, EntType, E](this)(extractRepo(input))(()=>this.action())
}

sealed trait EntityUpdateCommandG[F[_], -I <: CommandInput[F], E, IdType<:EntityId, EntType<:VersionedEntity[EntType,IdType]] extends EntityCommandG[F, I, EntType, E] with OptimisticLockingG {
  def id: IdType

  def version: EntityVersion

  def action(entity: EntType): Either[E, EntType]

  def staleF(id: IdType): E

  //def extractRepo[RepoType <: EntityRepo[F, IdType, EntType, E]](input: I): RepoType
  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, EntType]] =
    EntityRepoManager.manageUpdate[G, F, I, EntityUpdateCommandG[F,I,E,IdType,EntType], IdType, EntType, EntType, E](extractRepo(input))(this)(this.action, staleF)

  //  override def run[G[_], B](input: I)
  //                           (implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, InvoiceError]): G[Either[E, R]] = ???

  //  override def run[G[_]](input: I)
  //                        (implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[R, T, E]
  //                        ): G[Either[E, R]] =
  //    EntityRepoManager.manageUpdate(extractRepo(input))(this)(action, staleF)
}

sealed trait InvoiceCreateCommandG[F[_]] extends EntityCreateCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(): Either[InvoiceError, Invoice]

//  override def run[G[_], B](input: DomainCommandInput[F])
//                           (implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, InvoiceError]
//                           ): G[Either[InvoiceError, Invoice]] =
//    ??? //InvoiceRepoManager.manageCreate(input.repo)(this)(() => action())
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.repo
}

sealed trait InvoiceUpdateCommandG[F[_]] extends EntityUpdateCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(invoice: Invoice): Either[InvoiceError, Invoice]

  override def staleF(id: InvoiceId): InvoiceError = StaleError(id)

//  override def extractRepo[RepoType <: EntityRepo[F, InvoiceId, Invoice, InvoiceError]](input: DomainCommandInput[F]): RepoType = input.repo
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.repo

//  override def run[G[_], B](input: DomainCommandInput[F])
//                           (implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, InvoiceError]
//                           ): G[Either[InvoiceError, Invoice]] =
//    InvoiceRepoManager.manageUpdate(input.repo)(this)(action)
}

final case class ApproveCmdG[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.approveG[F](invoice, this)

  //override def staleF(id: InvoiceId): InvoiceError = ???

  //override def extractRepo[RepoType <: EntityRepo[F, InvoiceId, Invoice, InvoiceError]](input: DomainCommandInput[F]): RepoType = ???
  //override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = ???
}

final case class CreateRfiInvoiceCmdG[F[_]](userId: UserId) extends InvoiceCreateCommandG[F] {
  override def action(): Either[InvoiceError, Invoice] = Right(Invoice.createRfiInvoice(this))


}

final case class UpdateRfiCmdG[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.updateRfiG(invoice, this)

  //override def staleF(id: InvoiceId): InvoiceError = ???

  //override def extractRepo[RepoType <: EntityRepo[F, InvoiceId, Invoice, InvoiceError]](input: DomainCommandInput[F]): RepoType = ???
}
