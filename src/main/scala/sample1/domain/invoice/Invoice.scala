package sample1.domain.invoice

import sample1.domain.command.{CreateRfiInvoiceCmd, CreateRfiInvoiceCmdG}
import sample1.domain.entity.{EntityVersion, VersionedEntity}
import sample1.domain.{Cost, RequestForInvoice, UserId}

import scala.language.higherKinds

trait Invoice extends VersionedEntity[Invoice, InvoiceId] {
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
}
