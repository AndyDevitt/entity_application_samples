package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceAlgebraHelpers._
import sample1.domain.invoice.InvoiceStatus.{Assigned, Draft, NotApproved}
import sample1.domain.invoice._
import sample1.domain.permissions.{InvoiceEntityPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId
import sample1.domain.{Cost, NotAllowed}

object AddCost {

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

  // TODO: provide this action as an example of where the command state is required to check full permissions (i.e.
  //  cannot add costs to bring total over a certain limit)
  final case class AddCostCmdProcessor[F[_]]()
    extends InvoiceEntityCommandProcessor[F, SponsorInvoice, InvoiceAction.AddCost.type, AddCostCmd[F]] {

    val allowedStatuses: Set[InvoiceStatus] = Set(Draft, Assigned, NotApproved)
    val requiredPermissions: Set[InvoicePermissions] = Set(InvoicePermissions.Read, InvoicePermissions.AddCost)

    override def canDo(invoice: Invoice, action: InvoiceAction.AddCost.type, permissions: InvoiceUserPermissions
                      ): Either[NotAllowed, SponsorInvoice] =
      for {
        _ <- validateRequiredPermissions(permissions, requiredPermissions)
        sponsorInv <- validateSponsorInvoice(invoice)
        _ <- validateAllowedStatus(sponsorInv, allowedStatuses)
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

}
