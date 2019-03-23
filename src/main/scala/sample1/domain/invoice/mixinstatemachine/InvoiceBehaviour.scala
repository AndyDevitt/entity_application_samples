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
    case siteInvoice: SiteInvoice => siteInvoice.status match {
      case NotApproved =>
        new SiteInvoiceBehaviour(siteInvoice) // with Implementations.CanApprove
      case Approved =>
        SiteInvoiceBehaviour(siteInvoice)
    }
    case sponsorInvoice: SponsorInvoice => sponsorInvoice.status match {
      case NotApproved =>
        new SponsorInvoiceBehaviour(sponsorInvoice) with Implementations.CanApprove
      case Approved =>
        SponsorInvoiceBehaviour(sponsorInvoice)
    }
  }

  sealed trait BaseInvoiceBehaviour extends InvoiceBehaviour {
    def invoice: Invoice
  }

  case class SiteInvoiceBehaviour(override val invoice: SiteInvoice)
    extends BaseInvoiceBehaviour

  //with Implementations.CanApprove

  case class SponsorInvoiceBehaviour(override val invoice: SponsorInvoice)
    extends BaseInvoiceBehaviour

  trait InvoiceCommandProcessorMixin
    extends EntityCommandProcessorMixin[InvoiceAction, InvoiceError, Invoice, InvoiceUserPermissions] {
    override def notAllowedResult: Either[InvoiceError, Invoice] =
      Left(InvoiceError.ActionNotAllowedInCurrentStatus())
  }

  trait Approve extends InvoiceCommandProcessorMixin {
    def process[F[_]](cmd: ApproveCmdMixin[F], permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

  trait ApproveV2 extends InvoiceCommandProcessorMixin {
    def process[F[_]](cmd: ApproveCmdV2Mixin[F], permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

  trait UpdateRfi extends InvoiceCommandProcessorMixin {
    def process[F[_]](cmd: UpdateRfiCmdMixin[F], permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

}

object Implementations {

  import InvoiceStateBuilder.Instances._

  trait CanApprove extends InvoiceBehaviour.Approve {

    self: InvoiceBehaviour.BaseInvoiceBehaviour =>

    override def actionStatus(): Set[(InvoiceAction, ActionStatus)] =
      super.actionStatus() ++ thisActionStatus(InvoiceAction.Approve, self.invoice, validateActionIsAllowed)

    override def process[F[_]](cmd: ApproveCmdMixin[F],
                               permissions: InvoiceUserPermissions
                              ): Either[InvoiceError, Invoice] =
      validateActionIsAllowed(self.invoice) flatMap {
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
