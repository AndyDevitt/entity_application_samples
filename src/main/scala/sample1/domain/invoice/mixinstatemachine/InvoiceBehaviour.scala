package sample1.domain.invoice.mixinstatemachine

import sample1.domain.ActionStatus
import sample1.domain.command._
import sample1.domain.entity.mixinstatemachine.{Actions, EntityCommandProcessorMixin}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.permissions.InvoiceUserPermissions

trait InvoiceBehaviour
  extends Actions[Invoice, InvoiceAction]
    with InvoiceBehaviour.Approve
    with InvoiceBehaviour.ApproveV2
    with InvoiceBehaviour.UpdateRfi

object InvoiceBehaviour {

  def apply(invoice: Invoice): InvoiceBehaviour = invoice match {
    case siteInvoice: SiteInvoice => SiteInvoiceBehaviour(siteInvoice)
    case sponsorInvoice: SponsorInvoice => SponsorInvoiceBehaviour(sponsorInvoice)
  }

  final case class SiteInvoiceBehaviour(siteInvoice: SiteInvoice)
    extends InvoiceBehaviour

  //with Implementations.CanApprove

  final case class SponsorInvoiceBehaviour(sponsorInvoice: SponsorInvoice)
    extends InvoiceBehaviour
      with Implementations.CanApprove

  trait InvoiceCommandProcessorMixin
    extends EntityCommandProcessorMixin[InvoiceAction, InvoiceError, Invoice, InvoiceUserPermissions] {
    override def notAllowedResult: Either[InvoiceError, Invoice] =
      Left(InvoiceError.ActionNotAllowedInCurrentStatus())
  }

  trait Approve extends InvoiceCommandProcessorMixin {
    def process[F[_]](invoice: Invoice, cmd: ApproveCmdMixin[F], permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

  trait ApproveV2 extends InvoiceCommandProcessorMixin {
    def process[F[_]](invoice: Invoice, cmd: ApproveCmdV2Mixin[F], permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

  trait UpdateRfi extends InvoiceCommandProcessorMixin {
    def process[F[_]](invoice: Invoice, cmd: UpdateRfiCmdMixin[F], permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

}

object Implementations {

  import InvoiceStateBuilder.Instances._

  trait CanApprove extends InvoiceBehaviour.Approve {

    self: InvoiceBehaviour.SponsorInvoiceBehaviour =>

    override def actionStatus(invoice: Invoice): Set[(InvoiceAction, ActionStatus)] =
      super.actionStatus(invoice) ++ thisActionStatus(InvoiceAction.Approve, invoice, validateActionIsAllowed)

    override def process[F[_]](invoice: Invoice,
                               cmd: ApproveCmdMixin[F],
                               permissions: InvoiceUserPermissions
                              ): Either[InvoiceError, Invoice] =
      validateActionIsAllowed(invoice) map {
        InvoiceStateBuilder.Builder(_)
          .clearCosts()
          .updateLastEdited(cmd)
          .build()
      }

    private def validateActionIsAllowed(invoice: Invoice): Either[InvoiceError, SponsorInvoice] = invoice match {
      case _: SiteInvoice =>
        Left(InvoiceError.ActionNotAllowedForProcessType())
      case sponsorInvoice: SponsorInvoice =>
        Either.cond(invoice.costs.nonEmpty, sponsorInvoice, InvoiceError.CannotApproveWithoutCosts())
    }

  }

}
