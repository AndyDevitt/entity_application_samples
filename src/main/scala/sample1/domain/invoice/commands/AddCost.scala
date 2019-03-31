package sample1.domain.invoice.commands

import sample1.domain.Cost
import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceStatus.{Assigned, Draft, NotApproved}
import sample1.domain.invoice._
import sample1.domain.permissions.{InvoiceEntityPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId

object AddCost {

  final case class AddCostCmd[F[_]](userId: UserId,
                                    id: InvoiceId,
                                    version: EntityVersion,
                                    permissionsRetriever: InvoiceEntityPermissionRetriever[F],
                                    cost: Cost
                                   ) extends InvoiceUpdateCommand[F, AddCostCmd[F], InvoiceAction.AddCost.type] {
    override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      AddCostCmdProcessor().process(invoice, this, permissions)
  }

  // TODO: provide this action as an example of where the command state is required to check full permissions (i.e.
  //  cannot add costs to bring total over a certain limit)
  final case class AddCostCmdProcessor[F[_]]()
    extends SponsorInvoiceCommandProcessor[F, InvoiceAction.AddCost.type, AddCostCmd[F]] {

    override val allowedStatuses: Set[InvoiceStatus] = Set(Draft, Assigned, NotApproved)
    override val requiredPermissions: Set[InvoicePermissions] = Set(InvoicePermissions.Read, InvoicePermissions.AddCost)

    override protected def action(entity: SponsorInvoice, cmd: AddCostCmd[F], permissions: InvoiceUserPermissions
                                 ): Either[InvoiceError, Invoice] =
      for {
        _ <- Either.cond(
          entity.costs.forall(c => c.amount.currency == cmd.cost.amount.currency),
          (),
          InvoiceError.CannotAddCostsWithDifferentCurrencies())
        updated <- InvoiceStateBuilder.Builder(entity)
          .updateLastEdited(cmd)
          .addCost(cmd.cost)
          .map(_.build())
      } yield updated
  }

}
