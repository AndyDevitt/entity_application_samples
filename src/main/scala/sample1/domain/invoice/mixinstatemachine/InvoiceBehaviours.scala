package sample1.domain.invoice.mixinstatemachine

import sample1.domain.ActionStatus
import sample1.domain.command._
import sample1.domain.entity.VersionedEntity
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.permissions.InvoiceUserPermissions

trait InvoiceBehaviours
  extends InvoiceBehaviours.Approve
    with InvoiceBehaviours.ApproveV2
    with InvoiceBehaviours.UpdateRfi

trait EntityBehaviour[ActionType, ErrType, EntType <: VersionedEntity[_], PermissionType] {

  import scala.language.implicitConversions
  //  def entityState: EntType

  def actionStatus: Set[(ActionType, ActionStatus)] = Set()

  def notAllowedResult: Either[ErrType, EntType]

  // A very localised use of a helper implicit conversion so that Jacob wouldn't have to keep writing Right(...) :-)
  implicit def convertToSuccess(entity: EntType): Either[ErrType, EntType] = Right(entity)
}

object InvoiceBehaviours {

  def apply(invoice: Invoice): InvoiceBehaviours = invoice match {
    case siteInvoice: SiteInvoice => SiteInvoiceBehaviour(siteInvoice)
    case sponsorInvoice: SponsorInvoice => SponsorInvoiceBehaviour(sponsorInvoice)
  }

  final case class SiteInvoiceBehaviour(siteInvoice: SiteInvoice)
    extends InvoiceBehaviours
      with Implementations.CanApprove

  final case class SponsorInvoiceBehaviour(sponsorInvoice: SponsorInvoice)
    extends InvoiceBehaviours
      with Implementations.CanApprove

  trait InvoiceBehaviour
    extends EntityBehaviour[InvoiceAction, InvoiceError, Invoice, InvoiceUserPermissions] {
    override def notAllowedResult: Either[InvoiceError, Invoice] =
      Left(InvoiceError.ActionNotAllowedInCurrentStatus())
  }

  trait Approve extends InvoiceBehaviour {
    def process[F[_]](invoice: Invoice,
                      cmd: ApproveCmdMixin[F],
                      permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

  trait ApproveV2 extends InvoiceBehaviour {
    def process[F[_]](invoice: Invoice,
                      cmd: ApproveCmdV2Mixin[F],
                      permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

  trait UpdateRfi extends InvoiceBehaviour {
    def process[F[_]](invoice: Invoice,
                      cmd: UpdateRfiCmdMixin[F],
                      permissions: InvoiceUserPermissions
                     ): Either[InvoiceError, Invoice] =
      notAllowedResult
  }

}

object Implementations {

  import InvoiceStateBuilder.Instances._

  trait CanApprove extends InvoiceBehaviours.Approve {
    override def process[F[_]](invoice: Invoice,
                               cmd: ApproveCmdMixin[F],
                               permissions: InvoiceUserPermissions
                              ): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(invoice)
        .clearCosts()
        .updateLastEdited(cmd)
        .build()
  }

}

object InvoiceBehaviourWrappers {

  def apply(invoice: Invoice): InvoiceBehaviours = invoice match {
    case siteInvoice: SiteInvoice => SiteInvoiceBehaviour(siteInvoice)
    case sponsorInvoice: SponsorInvoice => SponsorInvoiceBehaviour(sponsorInvoice)
  }

  final case class SiteInvoiceBehaviour(siteInvoice: SiteInvoice)
    extends InvoiceBehaviours
      with Implementations.CanApprove

  final case class SponsorInvoiceBehaviour(sponsorInvoice: SponsorInvoice)
    extends InvoiceBehaviours
      with Implementations.CanApprove

}
