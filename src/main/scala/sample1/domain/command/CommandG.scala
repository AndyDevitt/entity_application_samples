package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain._
import sample1.domain.entity._
import sample1.domain.invoice._

import scala.language.higherKinds

trait CommandInput {
  //def repo: InvoiceRepo[F]
}

class DomainCommandInput[F[_]](val repo: EntityRepo[F, InvoiceId, Invoice, InvoiceError]) extends CommandInput

trait CommandRunner[F[_], G[_], C <: CommandG[F, I, R, E], -I <: CommandInput, R, E] {
  def run(command: C, input: I): G[Either[E, R]]
}

object CommandRunner {

  implicit def invoiceUpdateCommandRunner[F[_], G[_]](implicit transformer: F ~> G, monadF: Monad[F]
                                                     ): CommandRunner[F, G, InvoiceUpdateCommandG[F], DomainCommandInput[F], Invoice, InvoiceError] =
    ??? //(command: InvoiceUpdateCommandG[F], input: DomainCommandInput[F]) => InvoiceRepoManager.manageUpdate(input.repo)(command)(command.action)

  //  implicit val idInvoiceUpdateCommandRunner = invoiceUpdateCommandRunner[Id,Id]
}


trait CommandG[F[_], -I <: CommandInput, R, E] extends Command {
  def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, R]]
}

sealed trait EntityCommandG[F[_], -I <: CommandInput, R, E] extends CommandG[F, I, R, E]

sealed trait OptimisticLockingG {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLockingG extends OptimisticLockingG {
  override def enforceOptimisticLocking: Boolean = false
}

sealed trait EntityCreateCommandG[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType]] extends EntityCommandG[F, I, EntType, E] {
  def action(): Either[E, EntType]

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, EntType]] =
    EntityRepoManager.manageCreate[G, F, I, EntityCreateCommandG[F, I, E, IdType, EntType], IdType, EntType, EntType, E](extractRepo(input))(this)(() => this.action())
}

sealed trait EntityUpdateCommandG[F[_], -I <: CommandInput, E, IdType <: EntityId, EntType <: VersionedEntity[EntType, IdType]] extends EntityCommandG[F, I, EntType, E] with OptimisticLockingG {
  def id: IdType

  def version: EntityVersion

  def action(entity: EntType): Either[E, EntType]

  def staleF(id: IdType): E

  def extractRepo(input: I): EntityRepo[F, IdType, EntType, E]

  override def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, E]): G[Either[E, EntType]] =
    EntityRepoManager.manageUpdate[G, F, I, EntityUpdateCommandG[F, I, E, IdType, EntType], IdType, EntType, EntType, E](extractRepo(input))(this)(this.action, staleF)
}

sealed trait InvoiceCreateCommandG[F[_]] extends EntityCreateCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(): Either[InvoiceError, Invoice]

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.repo
}

sealed trait InvoiceUpdateCommandG[F[_]] extends EntityUpdateCommandG[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice] {
  def action(invoice: Invoice): Either[InvoiceError, Invoice]

  override def staleF(id: InvoiceId): InvoiceError = StaleError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] = input.repo
}

final case class ApproveCmdG[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.approveG[F](invoice, this)
}

final case class CreateRfiInvoiceCmdG[F[_]](userId: UserId) extends InvoiceCreateCommandG[F] {
  override def action(): Either[InvoiceError, Invoice] = Right(Invoice.createRfiInvoice(this))
}

final case class UpdateRfiCmdG[F[_]](userId: UserId, id: InvoiceId, version: EntityVersion) extends InvoiceUpdateCommandG[F] {
  override def action(invoice: Invoice): Either[InvoiceError, Invoice] = InvoiceAlgebra.updateRfiG(invoice, this)
}
