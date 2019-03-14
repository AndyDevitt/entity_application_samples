package sample1.domain.invoice

import sample1.domain.command.{CreateRfiInvoiceCmd, CreateSiteInvoiceCmd}
import sample1.domain.entity.{EntityVersion, VersionedEntity}
import sample1.domain.user.UserId
import sample1.domain.{Cost, RequestForInvoice}

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
  def createRfiInvoice[F[_]](cmd: CreateRfiInvoiceCmd[F]): SponsorInvoice =
    SponsorInvoice(InvoiceId(), EntityVersion(), cmd.userId, NotApproved, Nil, RequestForInvoice())

  def createRfiInvoiceEmpty[F[_]](): SponsorInvoice =
    SponsorInvoice(InvoiceId(), EntityVersion(), UserId(), NotApproved, Nil, RequestForInvoice())

  def createSiteInvoice[F[_]](cmd: CreateSiteInvoiceCmd[F]): SiteInvoice =
    SiteInvoice(InvoiceId(), EntityVersion(), cmd.userId, NotApproved, Nil)

  def createSiteInvoiceEmpty[F[_]](): SiteInvoice =
    SiteInvoice(InvoiceId(), EntityVersion(), UserId(), NotApproved, Nil)
}

object SiteInvoice {
  private[invoice] def apply(id: InvoiceId,
                             version: EntityVersion,
                             lastEditedBy: UserId,
                             status: InvoiceStatus,
                             costs: List[Cost]): SiteInvoice = new SiteInvoice(id, version, lastEditedBy, status, costs)
}

object SponsorInvoice {
  private[invoice] def apply(id: InvoiceId,
                             version: EntityVersion,
                             lastEditedBy: UserId,
                             status: InvoiceStatus,
                             costs: List[Cost],
                             rfi: RequestForInvoice): SponsorInvoice = new SponsorInvoice(id, version, lastEditedBy, status, costs, rfi)
}
