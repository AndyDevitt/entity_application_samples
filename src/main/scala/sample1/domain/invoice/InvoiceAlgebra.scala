package sample1.domain.invoice

import sample1.domain._
import sample1.domain.errors.{InvoiceError, ValidationError}
import sample1.domain.invoice.commands._
import sample1.domain.permissions.{InvoicePermissions, InvoiceUserPermissions}
import sample1.utils.ReduceOptionWithFailure._


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
      case a: InvoiceAction.Approve.type => ApproveCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.UpdateRfi.type => UpdateRfiCmdProcessor().actionStatus(invoice, a, permissions)
      case a: InvoiceAction.AddCost.type => AddCostCmdProcessor().actionStatus(invoice, a, permissions)
    }

  def calculateTotal(invoice: Invoice): Either[ValidationError, Option[MonetaryAmount]] =
    invoice.costs
      .map(_.amount)
      .reduceOptionWithFailure(MonetaryAmountAlg.sum)

}
