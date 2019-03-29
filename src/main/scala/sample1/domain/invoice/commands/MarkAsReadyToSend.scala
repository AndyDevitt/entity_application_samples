package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceStatus.Assigned
import sample1.domain.invoice._
import sample1.domain.permissions.{EntityPermissionsRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId

object MarkAsReadyToSend {

  final case class MarkAsReadyToSendCmd[F[_]](userId: UserId,
                                              id: InvoiceId,
                                              version: EntityVersion,
                                              permissionsRetriever: EntityPermissionsRetriever[F, InvoiceId, Invoice, InvoiceUserPermissions])
    extends InvoiceUpdateCommand[F, MarkAsReadyToSendCmd[F], InvoiceAction.MarkAsReadyToSend.type] {
    override val associatedAction: InvoiceAction.MarkAsReadyToSend.type = InvoiceAction.MarkAsReadyToSend

    override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      MarkAsReadyToSendCmdProcessor().process(invoice, this, permissions)
  }

  final case class MarkAsReadyToSendCmdProcessor[F[_]]()
    extends InvoiceCommandProcessor[F, InvoiceAction.MarkAsReadyToSend.type, MarkAsReadyToSendCmd[F]] {

    override val allowedStatuses: Set[InvoiceStatus] = Set(Assigned)
    override val requiredPermissions: Set[InvoicePermissions] = Set()

    override protected def action(entity: Invoice, cmd: MarkAsReadyToSendCmd[F], permissions: InvoiceUserPermissions
                                 ): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(entity)
        .setStatus(InvoiceStatus.ReadyToSend)
        .updateLastEdited(cmd)
        .build()
  }

}
