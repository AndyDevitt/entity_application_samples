package sample1.domain.invoice

import sample1.domain._
import sample1.domain.command._
import scala.language.higherKinds

object InvoiceAlgebra {

  import cats.data.State
  import sample1.domain.invoice.InvoiceUtils._

  def actionFromCommand(cmd: Command): InvoiceAction = cmd match {
    case _: ApproveCmd => InvoiceAction.Approve
    case _: CreateRfiInvoiceCmd => InvoiceAction.CreateRfi
    case _: UpdateRfiCmd => InvoiceAction.UpdateRfi
  }

  def actionStatuses(invoice: Invoice): Set[ActionStatus] =
    EnumerableAdt[InvoiceAction].map(actionStatus(invoice, _))

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
    case c: EntityUpdateCommand[_] if c.enforceOptimisticLocking && c.version != invoice.version => Left(StaleInvoiceError(invoice.id))
    case _ => Right(invoice)
  }

  def canCreateRfi(invoice: Invoice): Either[NotAllowed, SponsorInvoice] = invoice match {
    case si: SponsorInvoice => Right(si)
    case _: SiteInvoice => Left(NotAllowedForProcessType())
  }

  def createRfi(invoice: Invoice, cmd: CreateRfiInvoiceCmd): Either[InvoiceError, Invoice] =
    canDoAction(canCreateRfi)(invoice, cmd) map { si =>
      val pgm = for {
        _ <- State[SponsorInvoice, Unit] { s => (InvoiceUtils.createRfi(s, cmd), ()) }
      } yield ()
      pgm.runS(si).value
    }

  def canApprove(invoice: Invoice): Either[NotAllowed, Invoice] = Right(invoice)

  def approve(invoice: Invoice, cmd: ApproveCmd): Either[InvoiceError, Invoice] =
    canDoAction(canApprove)(invoice, cmd) map { inv =>
      val pgm = for {
        _ <- State[Invoice, Unit] { s => (clearCosts(s, cmd), ()) }
        _ <- State[Invoice, Unit] { s => (setStatus(s, cmd, Approved), ()) }
      } yield ()
      pgm.runS(inv).value
    }

  def approveG[F[_]](invoice: Invoice, cmd: ApproveCmdG[F]): Either[InvoiceError, Invoice] =
    canDoAction(canApprove)(invoice, cmd) map { inv =>
      val pgm = for {
        _ <- State[Invoice, Unit] { s => (clearCosts(s, cmd), ()) }
        _ <- State[Invoice, Unit] { s => (setStatus(s, cmd, Approved), ()) }
      } yield ()
      pgm.runS(inv).value
    }

  def approve2(invoice: Invoice, cmd: ApproveCmd): Either[InvoiceError, Invoice] =
    canDoAction(canApprove)(invoice, cmd) map { inv =>
      val pgm = for {
        _ <- InvoiceStateUtils.clearCosts(inv, cmd)
        _ <- InvoiceStateUtils.setStatus(inv, cmd, Approved)
      } yield ()
      pgm.runS(inv).value
    }

  def canUpdateRfi(invoice: Invoice): Either[NotAllowed, SponsorInvoice] = invoice match {
    case si: SponsorInvoice if Set(NotApproved).exists(_ == si.status) => Right(si)
    case si: SponsorInvoice => Left(NotAllowedInCurrentStatus())
    case _: SiteInvoice => Left(NotAllowedForProcessType())
  }

  def updateRfi(invoice: Invoice, cmd: UpdateRfiCmd): Either[InvoiceError, Invoice] =
    canDoAction(canUpdateRfi)(invoice, cmd) map { inv =>
      val pgm = for {
        _ <- InvoiceStateUtils.clearCosts(inv, cmd)
        _ <- InvoiceStateUtils.setStatus(inv, cmd, Approved)
        _ <- InvoiceStateUtils.updateRfi(inv, cmd, RequestForInvoice())
      } yield ()
      pgm.runS(inv).value
    }

  def updateRfiG[F[_]](invoice: Invoice, cmd: UpdateRfiCmdG[F]): Either[InvoiceError, Invoice] =
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
