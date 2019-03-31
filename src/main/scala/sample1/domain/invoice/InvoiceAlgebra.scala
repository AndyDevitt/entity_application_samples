package sample1.domain.invoice

import sample1.domain._
import sample1.domain.errors.{InvoiceError, ValidationError}
import sample1.domain.invoice.commands._
import sample1.domain.permissions.{InvoicePermissions, InvoiceUserPermissions}
import sample1.utils.ReduceOptionWithFailure._

object InvoiceAlgebra {

  def minimumAccessPermissionsCheck(invoice: Invoice, permissions: InvoiceUserPermissions
                                   ): Either[InvoiceError, Unit] =
    for {
      _ <- Either.cond(permissions.hasReadPermission, (), InvoiceError.AccessDenied())
      _ <- invoice match {
        case _: SiteInvoice => Either.cond(permissions.has(InvoicePermissions.ReadSiteInvoice), (), InvoiceError.AccessDenied())
        case _: SponsorInvoice => Either.cond(permissions.has(InvoicePermissions.ReadSponsorInvoice), (), InvoiceError.AccessDenied())
      }
    } yield ()

  def actionStatuses_old(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    EnumerableAdt[InvoiceAction].map(action => (action, actionStatus(invoice, action, permissions)))

  sealed trait MyActions

  case object Action1 extends MyActions

  case object Action2 extends MyActions

  implicit val action1Resolver: RetrieveActionStatus[Action1.type, Invoice, InvoiceUserPermissions] =
    (action: Action1.type, entity: Invoice, permissions: InvoiceUserPermissions) => ???

  implicit val action2Resolver: RetrieveActionStatus[Action2.type, Invoice, InvoiceUserPermissions] =
    (action: Action2.type, entity: Invoice, permissions: InvoiceUserPermissions) => ???

  //  implicit def actionStatusEnumerator[C<:Coproduct](implicit
  //                                                    gen: Generic.Aux[MyActions, C],
  //                                                    allSingletonsVisitor: AllSingletonsVisitor[MyActions, C, Invoice, InvoiceUserPermissions]
  //                                                   ): ActionStatusEnumerator[MyActions,Invoice,InvoiceUserPermissions] = new ActionStatusEnumerator[MyActions,Invoice,InvoiceUserPermissions] {
  //    override def actionStatuses(entity: Invoice, permissions: InvoiceUserPermissions): Set[(MyActions, ActionStatus)] = ???
  //  }

  implicit val sendResolver: RetrieveActionStatus[InvoiceAction.Send.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.Send.type, entity: Invoice, permissions: InvoiceUserPermissions) => (action, Send.SendCmdProcessor().actionStatus(entity, action, permissions))
  implicit val withdrawResolver: RetrieveActionStatus[InvoiceAction.Withdraw.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.Withdraw.type, entity: Invoice, permissions: InvoiceUserPermissions) => (action, Withdraw.WithdrawCmdProcessor().actionStatus(entity, action, permissions))
  implicit val assingToPayeeResolver: RetrieveActionStatus[InvoiceAction.AssignToPayee.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.AssignToPayee.type, entity: Invoice, permissions: InvoiceUserPermissions) => (action, AssignToPayee.AssignToPayeeCmdProcessor().actionStatus(entity, action, permissions))
  implicit val markAsReadyToSendResolver: RetrieveActionStatus[InvoiceAction.MarkAsReadyToSend.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.MarkAsReadyToSend.type, entity: Invoice, permissions: InvoiceUserPermissions) => (action, MarkAsReadyToSend.MarkAsReadyToSendCmdProcessor().actionStatus(entity, action, permissions))
  implicit val addCostResolver: RetrieveActionStatus[InvoiceAction.AddCost.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.AddCost.type, entity: Invoice, permissions: InvoiceUserPermissions) => (action, AddCost.AddCostCmdProcessor().actionStatus(entity, action, permissions))
  implicit val approveResolver: RetrieveActionStatus[InvoiceAction.Approve.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.Approve.type, entity: Invoice, permissions: InvoiceUserPermissions) => (action, Approve.ApproveCmdProcessor().actionStatus(entity, action, permissions))
  implicit val updateRfiResolver: RetrieveActionStatus[InvoiceAction.UpdateRfi.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.UpdateRfi.type, entity: Invoice, permissions: InvoiceUserPermissions) => (action, UpdateRfi.UpdateRfiCmdProcessor().actionStatus(entity, action, permissions))

  def actionStatuses(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    ActionStatusEnumerator[InvoiceAction, Invoice, InvoiceUserPermissions](invoice, permissions)

  private def actionStatus(invoice: Invoice, action: InvoiceAction, permissions: InvoiceUserPermissions): ActionStatus =
    action match {
      case a: InvoiceAction.Approve.type => Approve.ApproveCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.UpdateRfi.type => UpdateRfi.UpdateRfiCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.AddCost.type => AddCost.AddCostCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.AssignToPayee.type => AssignToPayee.AssignToPayeeCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.MarkAsReadyToSend.type => MarkAsReadyToSend.MarkAsReadyToSendCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.Withdraw.type => Withdraw.WithdrawCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.Send.type => Send.SendCmdProcessor().actionStatus(invoice, a, permissions)
    }

  def calculateTotal(invoice: Invoice): Either[ValidationError, Option[MonetaryAmount]] =
    invoice.costs
      .map(_.amount)
      .reduceOptionWithFailure(MonetaryAmountAlg.sum)

}
