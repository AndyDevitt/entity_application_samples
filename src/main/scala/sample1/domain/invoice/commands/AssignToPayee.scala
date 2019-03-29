package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceStatus.Draft
import sample1.domain.invoice._
import sample1.domain.permissions.{EntityPermissionsRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId
import sample1.domain.{ActionStatus, NotAllowed}

object AssignToPayee {

  final case class AssignToPayeeCmd[F[_]](userId: UserId,
                                          id: InvoiceId,
                                          version: EntityVersion,
                                          permissionsRetriever: EntityPermissionsRetriever[F, InvoiceId, Invoice, InvoiceUserPermissions])
    extends InvoiceUpdateCommand[F, AssignToPayeeCmd[F], InvoiceAction.AssignToPayee.type] {
    override val associatedAction: InvoiceAction.AssignToPayee.type = InvoiceAction.AssignToPayee

    override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      AssignToPayeeCmdProcessor().process(invoice, this, permissions)
  }

  final case class AssignToPayeeCmdProcessor[F[_]]()
    extends InvoiceEntityCommandProcessor[F, Invoice, InvoiceAction.AssignToPayee.type, AssignToPayeeCmd[F]] {

    private val allowedStatuses: Set[InvoiceStatus] = Set(Draft)

    override protected def canDo(entity: Invoice,
                                 action: InvoiceAction.AssignToPayee.type,
                                 permissions: InvoiceUserPermissions
                                ): Either[NotAllowed, Invoice] =
      Either.cond(allowedStatuses.contains(entity.status), entity, ActionStatus.NotAllowedInCurrentStatus())

    override protected def action(entity: Invoice, cmd: AssignToPayeeCmd[F], permissions: InvoiceUserPermissions
                                 ): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(entity)
        .setStatus(InvoiceStatus.Assigned)
        .updateLastEdited(cmd)
        .build()
  }

}
