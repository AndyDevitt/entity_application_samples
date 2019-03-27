package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceAlgebraHelpers.{isInOneOfStatus, validateSponsorInvoice}
import sample1.domain.invoice.InvoiceStateBuilder.Instances._
import sample1.domain.invoice._
import sample1.domain.permissions.{InvoiceEntityPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId
import sample1.domain.{ActionStatus, Cost, NotAllowed}

final case class AddCostCmd[F[_]](userId: UserId,
                                  id: InvoiceId,
                                  version: EntityVersion,
                                  permissionsRetriever: InvoiceEntityPermissionRetriever[F],
                                  cost: Cost
                                 ) extends InvoiceUpdateCommand[F, AddCostCmd[F], InvoiceAction.AddCost.type] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    AddCostCmdProcessor().process(invoice, this, permissions)

  override def associatedAction: InvoiceAction.AddCost.type = InvoiceAction.AddCost
}

// TODO: provide this action as an example of where the command state is required to check full permissions (i.e. cannot
//  add costs to bring total over a certain limit)
final case class AddCostCmdProcessor[F[_]]()
  extends InvoiceEntityCommandProcessor[F, SponsorInvoice, InvoiceAction.AddCost.type, AddCostCmd[F]] {

  val allowedStatuses: Set[InvoiceStatus] = Set(NotApproved)
  val requiredPermissions: Set[InvoicePermissions] = Set(InvoicePermissions.Read, InvoicePermissions.AddCost)

  override def canDo(invoice: Invoice, action: InvoiceAction.AddCost.type, permissions: InvoiceUserPermissions
                    ): Either[NotAllowed, SponsorInvoice] =
    for {
      _ <- Either.cond(
        permissions.hasAll(requiredPermissions),
        (),
        ActionStatus.NotEnoughPermissions(s"Not all permissions are present ($requiredPermissions)"))
      sponsorInv <- validateSponsorInvoice(invoice)
      _ <- Either.cond(isInOneOfStatus(sponsorInv, allowedStatuses), (), ActionStatus.NotAllowedInCurrentStatus())
    } yield sponsorInv

  override protected def action(entity: SponsorInvoice, cmd: AddCostCmd[F], permissions: InvoiceUserPermissions
                               ): Either[InvoiceError, Invoice] =
    for {
      _ <- Either.cond(
        entity.costs.forall(c => c.amount.currency == cmd.cost.amount.currency),
        (),
        InvoiceError.CannotAddCostsWithDifferentCurrencies())
      res <- InvoiceStateBuilder.Builder(entity)
        .updateLastEdited(cmd)
        .addCost(cmd.cost)
        .map(_.build())
    } yield res
}
