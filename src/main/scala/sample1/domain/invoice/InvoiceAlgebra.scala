package sample1.domain.invoice

import cats.syntax.either._
import sample1.domain._
import sample1.domain.command._
import sample1.domain.command.invoicecommands.{AddCostCmd, ApproveCmd, UpdateRfiCmd}
import sample1.domain.entity.EntityCommandProcessor
import sample1.domain.errors.{InvoiceError, ValidationError}
import sample1.domain.invoice.InvoiceStateBuilder.Instances._
import sample1.domain.permissions.{InvoicePermissions, InvoiceUserPermissions}
import sample1.utils.ReduceOptionWithFailure._
import sample1.domain.invoice.commands._

trait InvoiceEntityCommandProcessor[F[_], EntSubType <: Invoice, ActionType <: InvoiceAction, CmdType <: EntityUpdateCommand[F, _, InvoiceError, _, Invoice, InvoiceUserPermissions, InvoiceAction, ActionType]]
  extends EntityCommandProcessor[F, Invoice, EntSubType, InvoiceError, InvoiceUserPermissions, InvoiceAction, ActionType, CmdType, ActionStatus, NotAllowed] {

  override def statusToErrF: NotAllowed => InvoiceError = InvoiceError.fromActionStatus

  override def staleF: Invoice => InvoiceError = i => InvoiceError.StaleInvoiceError(i.id)

  override protected def minimumAccessPermissionsCheck(entity: Invoice, permissions: InvoiceUserPermissions): Either[NotAllowed, Unit] =
    InvoiceAlgebra.minimumAccessPermissionsCheck(entity, permissions).leftMap(_ => ActionStatus.AccessDenied())
}

object InvoiceAlgebraHelpers {

  def isInOneOfStatus(invoice: Invoice, statuses: Set[InvoiceStatus]): Boolean =
    statuses.contains(invoice.status)

  def validateSiteInvoice(invoice: Invoice): Either[NotAllowed, SiteInvoice] =
    invoice match {
      case i: SiteInvoice => Right(i)
      case _: SponsorInvoice => Left(ActionStatus.NotAllowedForProcessType())
    }

  def validateSponsorInvoice(invoice: Invoice): Either[NotAllowed, SponsorInvoice] =
    invoice match {
      case i: SponsorInvoice => Right(i)
      case _: SiteInvoice => Left(ActionStatus.NotAllowedForProcessType())
    }

}

object InvoiceAlgebra {

  import InvoiceAlgebraHelpers._
  import cats.data.State
  import sample1.domain.invoice.InvoiceUtils._

  def minimumAccessPermissionsCheck(invoice: Invoice, permissions: InvoiceUserPermissions
                                   ): Either[InvoiceError, Unit] =
    for {
      _ <- Either.cond(permissions.hasReadPermission, (), InvoiceError.AccessDenied())
      _ <- invoice match {
        case _: SiteInvoice => Either.cond(permissions.has(InvoicePermissions.ReadSiteInvoice), (), InvoiceError.AccessDenied())
        case _: SponsorInvoice => Either.cond(permissions.has(InvoicePermissions.ReadSponsorInvoice), (), InvoiceError.AccessDenied())
      }
    } yield ()

  def actionStatuses(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    EnumerableAdt[InvoiceAction].map(action => (action, actionStatus(invoice, action, permissions)))

  private def actionStatus(invoice: Invoice, action: InvoiceAction, permissions: InvoiceUserPermissions): ActionStatus =
    action match {
      case a: InvoiceAction.Approve.type => Approve.ApproveCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.UpdateRfi.type => UpdateRfi().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.AddCost.type => AddCost().actionStatus(invoice, a, permissions)
    }

  // TODO: provide this action as an example of where the command state is required to check full permissions (i.e. cannot
  // add costs to bring total over a certain limit)
  case class AddCost[F[_]]() extends InvoiceEntityCommandProcessor[F, SponsorInvoice, InvoiceAction.AddCost.type, AddCostCmd[F]] {

    val allowedStatuses: Set[InvoiceStatus] = Set(NotApproved)
    val requiredPermissions: Set[InvoicePermissions] = Set(InvoicePermissions.Read, InvoicePermissions.AddCost)

    override def canDo(invoice: Invoice, action: InvoiceAction.AddCost.type, permissions: InvoiceUserPermissions
                      ): Either[NotAllowed, SponsorInvoice] =
      for {
        _ <- Either.cond(permissions.hasAll(requiredPermissions), (), ActionStatus.NotEnoughPermissions(s"Not all permissions are present ($requiredPermissions)"))
        i <- validateSponsorInvoice(invoice)
        _ <- Either.cond(isInOneOfStatus(i, allowedStatuses), (), ActionStatus.NotAllowedInCurrentStatus())
      } yield i

    override protected def action(entity: SponsorInvoice, cmd: AddCostCmd[F], permissions: InvoiceUserPermissions
                                 ): Either[InvoiceError, Invoice] =
      for {
        _ <- Either.cond(entity.costs.forall(c => c.amount.currency == cmd.cost.amount.currency), (), InvoiceError.CannotAddCostsWithDifferentCurrencies())
        res <- InvoiceStateBuilder.Builder(entity)
          .updateLastEdited(cmd)
          .addCost(cmd.cost)
          .map(_.build())
      } yield res
  }

  case class Approve3[F[_]]() extends InvoiceEntityCommandProcessor[F, SponsorInvoice, InvoiceAction.Approve.type, ApproveCmd[F]] {
    override def canDo(invoice: Invoice, action: InvoiceAction.Approve.type, permissions: InvoiceUserPermissions): Either[NotAllowed, SponsorInvoice] = invoice match {
      case si: SponsorInvoice if Set(NotApproved).exists(_ == si.status) => Right(si)
      case si: SponsorInvoice => Left(ActionStatus.NotAllowedInCurrentStatus())
      case _: SiteInvoice => Left(ActionStatus.NotAllowedForProcessType())
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

  case class UpdateRfi[F[_]]() extends InvoiceEntityCommandProcessor[F, Invoice, InvoiceAction.UpdateRfi.type, UpdateRfiCmd[F]] {
    override def canDo(entity: Invoice, action: InvoiceAction.UpdateRfi.type, permissions: InvoiceUserPermissions): Either[NotAllowed, Invoice] = entity match {
      case si: SponsorInvoice if Set(NotApproved).exists(_ == si.status) => Right(si)
      case _: SponsorInvoice => Left(ActionStatus.NotAllowedInCurrentStatus())
      case _: SiteInvoice => Left(ActionStatus.NotAllowedForProcessType())
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
