package sample1.domain

import sample1.domain.entity.EntityRepo
import sample1.domain.invoice.{Invoice, InvoiceId}

trait InvoiceRepo[F[_]] extends EntityRepo[F, InvoiceId, Invoice, InvoiceError] {

}