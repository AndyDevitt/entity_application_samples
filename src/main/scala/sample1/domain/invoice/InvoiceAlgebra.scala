package sample1.domain.invoice

import sample1.domain._
import sample1.domain.command._
import sample1.domain.entity.EntityCommandProcessor
import sample1.domain.errors.{InvoiceError, ValidationError}
import sample1.domain.invoice.InvoiceStateBuilder.Instances._
import sample1.domain.permissions.InvoiceUserPermissions
import sample1.utils.ReduceOptionWithFailure._

trait InvoiceEntityCommandProcessor[F[_], EntSubType <: Invoice, ActionType <: InvoiceAction, CmdType <: EntityUpdateCommand[F, _, InvoiceError, _, Invoice, InvoiceUserPermissions, InvoiceAction, ActionType]]
  extends EntityCommandProcessor[F, Invoice, EntSubType, InvoiceError, InvoiceUserPermissions, InvoiceAction, ActionType, CmdType, ActionStatus, NotAllowed] {

  override def statusToErrF: NotAllowed => InvoiceError = InvoiceError.fromActionStatus

  override def staleF: Invoice => InvoiceError = i => InvoiceError.StaleInvoiceError(i.id)
}

object InvoiceAlgebra {

  import cats.data.State
  import sample1.domain.invoice.InvoiceUtils._

  def actionStatuses(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    EnumerableAdt[InvoiceAction].map(action => (action, actionStatus(invoice, action, permissions)))

  def actionStatus(invoice: Invoice, action: InvoiceAction, permissions: InvoiceUserPermissions): ActionStatus = {
    action match {
      case a: InvoiceAction.Approve.type => Approve3().canDo(invoice, a, permissions)
      case a: InvoiceAction.UpdateRfi.type => UpdateRfi().canDo(invoice, a, permissions)
    }
  }.fold[ActionStatus]((na: NotAllowed) => na, _ => Allowed)

  case class Approve3[F[_]]() extends InvoiceEntityCommandProcessor[F, SponsorInvoice, InvoiceAction.Approve.type, ApproveCmd[F]] {
    override def canDo(invoice: Invoice, action: InvoiceAction.Approve.type, permissions: InvoiceUserPermissions): Either[NotAllowed, SponsorInvoice] = invoice match {
      case si: SponsorInvoice if Set(NotApproved).exists(_ == si.status) => Right(si)
      case si: SponsorInvoice => Left(NotAllowedInCurrentStatus())
      case _: SiteInvoice => Left(NotAllowedForProcessType())
    }

    override protected def action(entity: SponsorInvoice, cmd: ApproveCmd[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] = {
      val pgm = for {
        _ <- State[SponsorInvoice, Unit] { s => (clearCosts(s, cmd), ()) }
        _ <- State[SponsorInvoice, Unit] { s => (setStatus(s, cmd, Approved), ()) }
      } yield ()
      pgm.runS(entity).value
    }
  }

  object Approve3 {
    def apply[F[_]](invoice: Invoice, cmd: ApproveCmd[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      new Approve3[F]().process(invoice, cmd, permissions)
  }

  case class ApproveV2[F[_]]() extends InvoiceEntityCommandProcessor[F, Invoice, InvoiceAction.Approve.type, ApproveCmdV2[F]] {
    override def canDo(entity: Invoice, action: InvoiceAction.Approve.type, permissions: InvoiceUserPermissions): Either[NotAllowed, Invoice] =
      Either.cond(entity.status == NotApproved, entity, NotAllowedInCurrentStatus())

    override protected def action(entity: Invoice, cmd: ApproveCmdV2[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(entity)
        .clearCosts()
        .setStatus(Approved)
        .updateLastEdited(cmd)
        .build()
  }

  case class UpdateRfi[F[_]]() extends InvoiceEntityCommandProcessor[F, Invoice, InvoiceAction.UpdateRfi.type, UpdateRfiCmd[F]] {
    override def canDo(entity: Invoice, action: InvoiceAction.UpdateRfi.type, permissions: InvoiceUserPermissions): Either[NotAllowed, Invoice] = entity match {
      case si: SponsorInvoice if Set(NotApproved).exists(_ == si.status) => Right(si)
      case _: SponsorInvoice => Left(NotAllowedInCurrentStatus())
      case _: SiteInvoice => Left(NotAllowedForProcessType())
    }

    override protected def action(entity: Invoice, cmd: UpdateRfiCmd[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(entity)
        .clearCosts()
        .setStatus(Approved)
        .updateLastEdited(cmd)
        .build()
  }

  def calculateTotal(invoice: Invoice): Either[ValidationError, Option[MonetaryAmount]] =
    invoice.costs
      .map(_.amount)
      .reduceOptionWithFailure(MonetaryAmountAlg.sum)

}
