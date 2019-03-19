package sample1.domain.invoice.mixinstatemachine

import sample1.domain.command._
import sample1.domain.entity.VersionedEntity
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.permissions.InvoiceUserPermissions
import sample1.domain.{ActionStatus, Allowed, NotAllowedInCurrentState}

trait InvoiceBehaviour
  extends Actions[Invoice, InvoiceAction]
    with InvoiceBehaviour.Approve
    with InvoiceBehaviour.ApproveV2
    with InvoiceBehaviour.UpdateRfi

trait Actions[EntType, ActionType] {
  def actionStatus(entity: EntType): Set[(ActionType, ActionStatus)]
}

trait EntityCommandProcessorMixin[ActionType, ErrType, EntType <: VersionedEntity[_], PermissionType] extends Actions[EntType, ActionType] {

  import scala.language.implicitConversions
  //  def entityState: EntType

  override def actionStatus(entity: EntType): Set[(ActionType, ActionStatus)] = Set.empty

  def notAllowedResult: Either[ErrType, EntType]

  // A very localised use of a helper implicit conversion so that Jacob wouldn't have to keep writing Right(...) :-)
  implicit def convertToSuccess(entity: EntType): Either[ErrType, EntType] = Right(entity)

  private def toActionStatus(checkResult: Either[ErrType, EntType]): ActionStatus =
    checkResult.fold[ActionStatus](e => NotAllowedInCurrentState(e.toString), _ => Allowed)

  protected def thisActionStatus(action: ActionType, entity: EntType, checkF: EntType => Either[ErrType, EntType]
                                ): Set[(ActionType, ActionStatus)] =
    Set((action, toActionStatus(checkF(entity))))
}

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
