package sample1.infrastructure

import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._

class InMemoryInvoiceRepo[F[_]] extends InMemoryRepo[F, InvoiceId, Invoice, InvoiceError] {
  override def notFoundErrorF: InvoiceId => InvoiceError = InvoiceError.InvoiceNotFound

  override def staleErrorF: InvoiceId => InvoiceError = InvoiceError.StaleInvoiceError
}
