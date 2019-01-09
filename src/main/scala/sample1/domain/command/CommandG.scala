package sample1.domain.command

import cats.{Monad, ~>}
import sample1.domain._
import sample1.domain.entity._
import sample1.domain.invoice._

import scala.language.higherKinds

trait CommandInput[F[_]] {
  def repo: InvoiceRepo[F]
}

class DomainCommandInput[F[_]](override val repo: InvoiceRepo[F]) extends CommandInput[F]

trait CommandG[F[_], -I <: CommandInput[F], R, E] extends Command {
  def run[G[_], B](input: I)(implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, InvoiceError]): G[Either[E, R]]
}

sealed trait EntityCommandG[F[_], -I <: CommandInput[F], R, E] extends CommandG[F, I, R, E]

sealed trait OptimisticLockingG {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLockingG extends OptimisticLockingG {
  override def enforceOptimisticLocking: Boolean = false
}

sealed trait EntityCreateCommandG[F[_], -I <: CommandInput[F], R, E, A <: EntityId] extends EntityCommandG[F, I, R, E]

sealed trait EntityUpdateCommandG[F[_], -I <: CommandInput[F], R, E, A <: EntityId] extends EntityCommandG[F, I, R, E] with OptimisticLockingG {
  def id: A

  def version: EntityVersion
}

sealed trait InvoiceCreateCommandG[F[_]] extends EntityCreateCommandG[F, DomainCommandInput[F], Invoice, InvoiceError, InvoiceId] {
  def action(): Either[InvoiceError, Invoice]

  override def run[G[_], B](input: DomainCommandInput[F])
                           (implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, InvoiceError]
                           ): G[Either[InvoiceError, Invoice]] =
    InvoiceRepoManager.manageCreate(input.repo)(this)(() => action())
}

sealed trait InvoiceUpdateCommandG[F[_]] extends EntityUpdateCommandG[F, DomainCommandInput[F], Invoice, InvoiceError, InvoiceId] {
  def action(invoice: Invoice): Either[InvoiceError, Invoice]

  override def run[G[_], B](input: DomainCommandInput[F])
                           (implicit monadF: Monad[F], transform: F ~> G, decoder: Decoder[B, Invoice, InvoiceError]
                           ): G[Either[InvoiceError, Invoice]] =
    InvoiceRepoManager.manageUpdate(input.repo)(this)(action)
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
