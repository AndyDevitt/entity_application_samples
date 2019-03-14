package sample1.domain.permissions

import sample1.domain.invoice.Invoice
import sample1.domain.user.UserId

import scala.concurrent.Future

class ProductionInvoicePermissionsRetriever extends InvoicePermissionsRetriever[Future] {
  def resolve(userId: UserId, entity: Invoice): Future[Set[InvoicePermissions]] =
    Future.successful(Set())
}
