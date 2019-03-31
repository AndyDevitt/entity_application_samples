package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceStatus.{Approved, NotApproved}
import sample1.domain.invoice._
import sample1.domain.permissions.{InvoiceEntityPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId

object UpdateRfi {

  final case class UpdateRfiCmd[F[_]](userId: UserId,
                                      id: InvoiceId,
                                      version: EntityVersion,
                                      permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                     ) extends InvoiceUpdateCommand[F, InvoiceAction.UpdateRfi.type] {
    override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      UpdateRfiCmdProcessor().process(invoice, this, permissions)
  }

  final case class UpdateRfiCmdProcessor[F[_]]()
    extends InvoiceCommandProcessor[F, InvoiceAction.UpdateRfi.type, UpdateRfiCmd[F]] {

    override protected def requiredPermissions: Set[InvoicePermissions] = Set()

    override protected def allowedStatuses: Set[InvoiceStatus] = Set(NotApproved)

    override protected def action(entity: Invoice,
                                  cmd: UpdateRfiCmd[F],
                                  permissions: InvoiceUserPermissions
                                 ): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(entity)
        .clearCosts()
        .setStatus(Approved)
        .updateLastEdited(cmd)
        .build()
  }

}
