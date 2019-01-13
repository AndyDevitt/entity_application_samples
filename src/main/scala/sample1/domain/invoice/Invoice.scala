package sample1.domain.invoice

import sample1.domain.command.{CreateRfiInvoiceCmd, CreateRfiInvoiceCmdG, CreateSiteInvoiceCmdG}
import sample1.domain.entity.{EntityVersion, VersionedEntity}
import sample1.domain.{Cost, RequestForInvoice, UserId}

trait Invoice extends VersionedEntity[InvoiceId] {
  def status: InvoiceStatus

  def costs: List[Cost]

  def lastEditedBy: UserId
}

final case class SiteInvoice(id: InvoiceId,
                             version: EntityVersion,
                             lastEditedBy: UserId,
                             status: InvoiceStatus,
                             costs: List[Cost]) extends Invoice

final case class SponsorInvoice(id: InvoiceId,
                                version: EntityVersion,
                                lastEditedBy: UserId,
                                status: InvoiceStatus,
                                costs: List[Cost],
                                rfi: RequestForInvoice) extends Invoice

object Invoice {
  def createRfiInvoice(cmd: CreateRfiInvoiceCmd): SponsorInvoice =
    SponsorInvoice(InvoiceId(), EntityVersion(), cmd.userId, NotApproved, Nil, RequestForInvoice())

  def createRfiInvoice[F[_]](cmd: CreateRfiInvoiceCmdG[F]): SponsorInvoice =
    SponsorInvoice(InvoiceId(), EntityVersion(), cmd.userId, NotApproved, Nil, RequestForInvoice())

  def createSiteInvoice[F[_]](cmd: CreateSiteInvoiceCmdG[F]): SiteInvoice =
    SiteInvoice(InvoiceId(), EntityVersion(), cmd.userId, NotApproved, Nil)
}
