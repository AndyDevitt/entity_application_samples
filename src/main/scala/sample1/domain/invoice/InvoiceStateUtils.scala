package sample1.domain.invoice

import cats.data.State
import sample1.domain._
import sample1.domain.command.Command

object InvoiceStateUtils {

  def clearCosts(invoice: Invoice, cmd: Command): State[Invoice, Unit] =
    State { s => (InvoiceUtils.clearCosts(s, cmd), ()) }

  def clearCosts(invoice: SiteInvoice, cmd: Command): State[SiteInvoice, Unit] =
    State { s => (InvoiceUtils.clearCosts(s, cmd), ()) }

  def clearCosts(invoice: SponsorInvoice, cmd: Command): State[SponsorInvoice, Unit] =
    State { s => (InvoiceUtils.clearCosts(s, cmd), ()) }

  def setStatus(invoice: SiteInvoice, cmd: Command, aStatus: InvoiceStatus): State[SiteInvoice, Unit] =
    State { s => (InvoiceUtils.setStatus(s, cmd, aStatus), ()) }

  def setStatus(invoice: SponsorInvoice, cmd: Command, aStatus: InvoiceStatus): State[SponsorInvoice, Unit] =
    State { s => (InvoiceUtils.setStatus(s, cmd, aStatus), ()) }

  def setStatus(invoice: Invoice, cmd: Command, aStatus: InvoiceStatus): State[Invoice, Unit] =
    State { s => (InvoiceUtils.setStatus(s, cmd, aStatus), ()) }

  def updateRfi(invoice: SponsorInvoice, cmd: Command, rfi: RequestForInvoice): State[SponsorInvoice, Unit] =
    State { s => (InvoiceUtils.updateRfi(s, cmd, rfi), ()) }

  //  def createRfi(invoice: SponsorInvoice, cmd: CreateRfiCmd): SponsorInvoice = invoice.copy(rfi = RequestForInvoice())

}
