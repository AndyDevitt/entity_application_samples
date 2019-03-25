package sample1.domain.invoice.mixinstatemachine

import sample1.domain.ActionStatus
import sample1.domain.command.invoicecommands.{ApproveCmdMixin, ApproveCmdV2Mixin, UpdateRfiCmdMixin}
import sample1.domain.entity.mixinstatemachine.{Actions, EntityCommandProcessorMixin}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.permissions.{InvoicePermissions, InvoiceUserPermissions}

trait InvoiceBehaviour
  extends Actions[Invoice, InvoiceAction, InvoiceUserPermissions]
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

  // TODO: Should permissions validation ALWAYS come before any other kind of check? Probably yes...
  // In which case the permissions check should also be performed in the default status, hence defined here as a
  // separate method and then called by the command when it is called to check minimum permissions...
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

    override def actionStatus(permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
      super.actionStatus(permissions) ++ thisActionStatus(InvoiceAction.Approve, self.invoice, permissions, validateActionIsAllowed)

    override def process[F[_]](cmd: ApproveCmdMixin[F],
                               permissions: InvoiceUserPermissions
                              ): Either[InvoiceError, Invoice] =
      validateActionIsAllowed(self.invoice, permissions) flatMap {
        InvoiceStateBuilder.Builder(_)
          .clearCosts()
          .updateLastEdited(cmd)
          .build()
      }

    private def validateActionIsAllowed(invoice: Invoice, permissions: InvoiceUserPermissions
                                       ): Either[InvoiceError, SponsorInvoice] =
      invoice match {
        case _: SiteInvoice =>
          Left(InvoiceError.ActionNotAllowedForProcessType())
        case sponsorInvoice: SponsorInvoice =>
          for {
            _ <- Either.cond(permissions.has(InvoicePermissions.Approve), (), InvoiceError.InsufficientPermissions("Approve permission not found for Approve command"))
            _ <- Either.cond(invoice.costs.nonEmpty, (), InvoiceError.CannotApproveWithoutCosts())
          } yield sponsorInvoice
      }

  }

}
