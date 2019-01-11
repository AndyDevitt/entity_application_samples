package sample1.infrastructure

import sample1.domain._
import sample1.domain.invoice._

import scala.language.higherKinds

class InMemoryInvoiceRepo[F[_]] extends InMemoryRepo[F, InvoiceId, Invoice, InvoiceError] {
  override def notFoundErrorF: InvoiceId => InvoiceError = InvoiceNotFound

  override def staleErrorF: InvoiceId => InvoiceError = StaleInvoiceError
}
