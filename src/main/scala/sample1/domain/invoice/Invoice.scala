package sample1.domain.invoice

import sample1.domain.Cost
import sample1.domain.entity.{EntityVersion, VersionedEntity}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.commands.CreateRfiInvoice.CreateRfiInvoiceCmd
import sample1.domain.invoice.commands.CreateSiteInvoice.CreateSiteInvoiceCmd
import sample1.domain.permissions.InvoiceUserPermissions
import sample1.domain.user.UserId

trait Invoice extends VersionedEntity[InvoiceId] {
  def status: InvoiceStatus

  def costs: List[Cost]

  def lastEditedBy: UserId
}

final case class SiteInvoice private(id: InvoiceId,
                                     version: EntityVersion,
                                     lastEditedBy: UserId,
                                     status: InvoiceStatus,
                                     costs: List[Cost]) extends Invoice

final case class SponsorInvoice private(id: InvoiceId,
                                        version: EntityVersion,
                                        lastEditedBy: UserId,
                                        status: InvoiceStatus,
                                        costs: List[Cost],
                                        rfi: RequestForInvoice) extends Invoice

object Invoice {
  def createRfiInvoice[F[_]](cmd: CreateRfiInvoiceCmd[F],
                             permissions: InvoiceUserPermissions): Either[InvoiceError, SponsorInvoice] =
    Either.cond(permissions.hasCreatePermission,
      SponsorInvoice(InvoiceId(), EntityVersion(), cmd.userId, NotApproved, Nil, RequestForInvoice()),
      InvoiceError.InsufficientPermissions("Create permission not found for update command"))


  def createRfiInvoiceEmpty[F[_]](): SponsorInvoice =
    SponsorInvoice(InvoiceId(), EntityVersion(), UserId(), NotApproved, Nil, RequestForInvoice())

  def createSiteInvoice[F[_]](cmd: CreateSiteInvoiceCmd[F],
                              permissions: InvoiceUserPermissions): Either[InvoiceError, SiteInvoice] =
    Either.cond(permissions.hasCreatePermission,
      SiteInvoice(InvoiceId(), EntityVersion(), cmd.userId, NotApproved, Nil),
      InvoiceError.InsufficientPermissions("Create permission not found for update command"))

  def createSiteInvoiceEmpty[F[_]](): SiteInvoice =
    SiteInvoice(InvoiceId(), EntityVersion(), UserId(), NotApproved, Nil)
}

object SiteInvoice {
  private[invoice] def apply(id: InvoiceId,
                             version: EntityVersion,
                             lastEditedBy: UserId,
                             status: InvoiceStatus,
                             costs: List[Cost]): SiteInvoice =
    new SiteInvoice(id, version, lastEditedBy, status, costs)
}

object SponsorInvoice {
  private[invoice] def apply(id: InvoiceId,
                             version: EntityVersion,
                             lastEditedBy: UserId,
                             status: InvoiceStatus,
                             costs: List[Cost],
                             rfi: RequestForInvoice): SponsorInvoice =
    new SponsorInvoice(id, version, lastEditedBy, status, costs, rfi)
}
