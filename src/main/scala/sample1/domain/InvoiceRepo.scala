package sample1.domain

import cats.Monad
import sample1.domain.invoice.{Invoice, InvoiceId}

import scala.language.higherKinds

trait InvoiceRepo[F[_]] {

  def save(invoice: Invoice)(implicit monad: Monad[F]): F[Either[InvoiceError, Invoice]]

  def retrieve(id: InvoiceId)(implicit monad: Monad[F]): F[Either[InvoiceError, Invoice]]

}
