package sample1.application

import sample1.domain._
import sample1.domain.entity.EntityVersion
import sample1.domain.invoice._
import sample1.domain.user.UserId

final case class InvoiceView private(id: InvoiceId,
                                     version: EntityVersion,
                                     status: InvoiceStatus,
                                     costs: List[Cost],
                                     lastEditedBy: UserId,
                                     requestForInvoice: Option[RequestForInvoice],
                                     total: Option[MonetaryAmount],
                                     actions: Set[(InvoiceAction, ActionStatus)]
                                    ) extends Invoice

object InvoiceView {
  def create(invoice: Invoice): Either[InvoiceError, InvoiceView] =
    for {
      total <- InvoiceAlgebra.calculateTotal(invoice)
    } yield new InvoiceView(
      id = invoice.id,
      version = invoice.version,
      status = invoice.status,
      costs = invoice.costs,
      lastEditedBy = invoice.lastEditedBy,
      requestForInvoice = rfi(invoice),
      total = total,
      actions = InvoiceAlgebra.actionStatuses(invoice)
    )

  private def rfi(invoice: Invoice): Option[RequestForInvoice] = invoice match {
    case i: SponsorInvoice => Some(i.rfi)
    case _: SiteInvoice => None
  }
}
