package sample1.domain.invoice

import sample1.domain._
import sample1.domain.command._

object InvoiceUtils {

  def asRight[A, E](obj: A): Either[E, A] = Right(obj)

  def clearCosts(invoice: Invoice, cmd: Command): Invoice = invoice match {
    case i: SiteInvoice => clearCosts(i, cmd: Command)
    case i: SponsorInvoice => clearCosts(i, cmd: Command)
  }

  def clearCosts(invoice: SiteInvoice, cmd: Command): SiteInvoice =
    updateLastEdited(invoice.copy(costs = Nil), cmd)

  def clearCosts(invoice: SponsorInvoice, cmd: Command): SponsorInvoice =
    updateLastEdited(invoice.copy(costs = Nil), cmd)

  def setStatus(invoice: SiteInvoice, cmd: Command, aStatus: InvoiceStatus): SiteInvoice =
    updateLastEdited(invoice.copy(status = aStatus), cmd)

  def setStatus(invoice: SponsorInvoice, cmd: Command, aStatus: InvoiceStatus): SponsorInvoice =
    updateLastEdited(invoice.copy(status = aStatus), cmd)

  def setStatus(invoice: Invoice, cmd: Command, aStatus: InvoiceStatus): Invoice = invoice match {
    case i: SiteInvoice => setStatus(i, cmd: Command, aStatus)
    case i: SponsorInvoice => setStatus(i, cmd: Command, aStatus)
  }

  def updateRfi(invoice: SponsorInvoice, cmd: Command, rfi: RequestForInvoice): SponsorInvoice =
    updateLastEdited(invoice.copy(rfi = rfi), cmd)

  def createRfi(invoice: SponsorInvoice, cmd: CreateRfiInvoiceCmd): SponsorInvoice =
    updateLastEdited(invoice.copy(rfi = RequestForInvoice()), cmd)

  private def updateLastEdited(invoice: SponsorInvoice, cmd: Command): SponsorInvoice =
    invoice.copy(lastEditedBy = cmd.userId)

  private def updateLastEdited(invoice: SiteInvoice, cmd: Command): SiteInvoice =
    invoice.copy(lastEditedBy = cmd.userId)

}