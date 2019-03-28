package sample1.domain.invoice

import sample1.domain.entity.EntityRepo
import sample1.domain.errors.InvoiceError

trait InvoiceRepo[F[_]] extends EntityRepo[F, InvoiceId, Invoice, InvoiceError] {
  def find(): F[Either[InvoiceError, List[Invoice]]]
}
