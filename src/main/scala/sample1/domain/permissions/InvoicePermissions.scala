package sample1.domain.permissions

import sample1.domain.permissions.InvoicePermissions.ApproveWithLimit

sealed trait InvoicePermissions

object InvoicePermissions {

  final case object Create extends InvoicePermissions

  final case object Read extends InvoicePermissions

  final case object ReadSiteInvoice extends InvoicePermissions

  final case object ReadSponsorInvoice extends InvoicePermissions

  final case object Approve extends InvoicePermissions

  final case object AssignToPayee extends InvoicePermissions

  final case object Withdraw extends InvoicePermissions

  final case object MarkReadyToSend extends InvoicePermissions

  final case object Reject extends InvoicePermissions

  final case object AddCost extends InvoicePermissions

  final case class ApproveWithLimit(limit: Int) extends InvoicePermissions

  final case object RunDomainServices extends InvoicePermissions

}

final case class InvoiceUserPermissions(permissions: Set[InvoicePermissions]) extends EntityPermissions[InvoicePermissions] {
  def hasReadPermission: Boolean = permissions.contains(InvoicePermissions.Read)

  def hasCreatePermission: Boolean = permissions.contains(InvoicePermissions.Create)

  def approvalLimit: Option[ApproveWithLimit] = permissions
    .collect({ case p: ApproveWithLimit => p })
    .reduceLeftOption { (acc: ApproveWithLimit, perm: ApproveWithLimit) =>
      if (acc.limit > perm.limit) acc else perm
    }
}
