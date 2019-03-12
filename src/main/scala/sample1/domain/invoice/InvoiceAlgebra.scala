package sample1.domain.invoice

import sample1.domain._
import sample1.domain.command._

object InvoiceAlgebra {

  import InvoiceStateBuilder.Instances._
  import InvoiceStateBuilder._
  import cats.data.State
  import sample1.domain.invoice.InvoiceUtils._

  def actionFromCommand(cmd: Command): InvoiceAction = cmd match {
    case _: ApproveCmd[_] => InvoiceAction.Approve
    case _: CreateRfiInvoiceCmd[_] => InvoiceAction.CreateRfi
    case _: UpdateRfiCmd[_] => InvoiceAction.UpdateRfi
  }

  def actionStatuses(invoice: Invoice): Set[(InvoiceAction, ActionStatus)] =
    EnumerableAdt[InvoiceAction].map(action => (action, actionStatus(invoice, action)))

  def actionStatus(invoice: Invoice, cmd: Command): ActionStatus =
    actionStatus(invoice, actionFromCommand(cmd))

  def actionStatus(invoice: Invoice, action: InvoiceAction): ActionStatus = {
    action match {
      case InvoiceAction.Approve => canApprove(invoice)
      case InvoiceAction.CreateRfi => canCreateRfi(invoice)
      case InvoiceAction.UpdateRfi => canUpdateRfi(invoice)
    }
  }.fold[ActionStatus]((na: NotAllowed) => na, _ => Allowed)

  private def canDoAction[A <: Invoice](f: Invoice => Either[NotAllowed, A])(invoice: Invoice, cmd: Command): Either[InvoiceError, A] =
    f(invoice)
      .left.map(InvoiceError.fromActionStatus)
      .flatMap(inv => checkOptimisticLocking(inv, cmd))

  private def checkOptimisticLocking[A <: Invoice](invoice: A, cmd: Command): Either[InvoiceError, A] = cmd match {
    case c: EntityUpdateCommand[_, _, _, _, _] if c.enforceOptimisticLocking && c.version != invoice.version => Left(StaleInvoiceError(invoice.id))
    case _ => Right(invoice)
  }

  def canCreateRfi(invoice: Invoice): Either[NotAllowed, SponsorInvoice] = invoice match {
    case si: SponsorInvoice => Right(si)
    case _: SiteInvoice => Left(NotAllowedForProcessType())
  }

  def createRfi[F[_]](invoice: Invoice, cmd: CreateRfiInvoiceCmd[F]): Either[InvoiceError, Invoice] =
    canDoAction(canCreateRfi)(invoice, cmd) map { InvoiceUtils.createRfi(_, cmd) }

  def canApprove(invoice: Invoice): Either[NotAllowed, Invoice] =
    Either.cond(invoice.status == NotApproved, invoice, NotAllowedInCurrentStatus())

  def approve[F[_]](invoice: Invoice, cmd: ApproveCmd[F]): Either[InvoiceError, Invoice] =
    canDoAction(canApprove)(invoice, cmd) map { inv =>
      val pgm = for {
        _ <- State[Invoice, Unit] { s => (clearCosts(s, cmd), ()) }
        _ <- State[Invoice, Unit] { s => (setStatus(s, cmd, Approved), ()) }
      } yield ()
      pgm.runS(inv).value
    }

  def approveV2[F[_]](invoice: Invoice, cmd: ApproveCmdV2[F]): Either[InvoiceError, Invoice] =
    canDoAction(canApprove)(invoice, cmd) map {
      _
        .clearCosts()
        .setStatus(Approved)
        .updateLastEdited(cmd)
        .build()
    }

  def canUpdateRfi(invoice: Invoice): Either[NotAllowed, SponsorInvoice] = invoice match {
    case si: SponsorInvoice if Set(NotApproved).exists(_ == si.status) => Right(si)
    case si: SponsorInvoice => Left(NotAllowedInCurrentStatus())
    case _: SiteInvoice => Left(NotAllowedForProcessType())
  }

  def updateRfi[F[_]](invoice: Invoice, cmd: UpdateRfiCmd[F]): Either[InvoiceError, Invoice] =
    canDoAction(canUpdateRfi)(invoice, cmd) map { inv =>
      val pgm = for {
        _ <- InvoiceStateUtils.clearCosts(inv, cmd)
        _ <- InvoiceStateUtils.setStatus(inv, cmd, Approved)
        _ <- InvoiceStateUtils.updateRfi(inv, cmd, RequestForInvoice())
      } yield ()
      pgm.runS(inv).value
    }

  def calculateTotal(invoice: Invoice): Either[ValidationError, Option[MonetaryAmount]] =
    reduceOptionWithFailure(invoice.costs.map(_.amount), MonetaryAmountAlg.sum)

  def reduceOptionWithFailure[A, E, B >: A](list: List[A], f: (B, A) => Either[E, B]): Either[E, Option[B]] =
    list.foldLeft(asRight[Option[B], E](Option.empty[B]))((b: Either[E, Option[B]], current) => b.flatMap {
      case Some(acc) => f(acc, current).map(Some(_))
      case None => Right(Some(current))
    })
}
