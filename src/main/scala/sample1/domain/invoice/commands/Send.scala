package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.{EntityVersion, RetrieveActionStatus}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.permissions.{EntityPermissionsRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId

object Send {

  final case class SendCmd[F[_]](userId: UserId,
                                 id: InvoiceId,
                                 version: EntityVersion,
                                 permissionsRetriever: EntityPermissionsRetriever[F, InvoiceId, Invoice, InvoiceUserPermissions]
                                )
    extends InvoiceUpdateCommand[F, InvoiceAction.Send.type] {
    override def action(invoice: Invoice,
                        permissions: InvoiceUserPermissions,
                       ): Either[InvoiceError, Invoice] =
      SendCmdProcessor().process(invoice, this, permissions)
  }

  implicit val sendResolver: RetrieveActionStatus[InvoiceAction.Send.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.Send.type, entity: Invoice, permissions: InvoiceUserPermissions) =>
      (action, Send.SendCmdProcessor().actionStatus(entity, action, permissions))

  final case class SendCmdProcessor[F[_]]()
    extends InvoiceCommandProcessor[F, InvoiceAction.Send.type, SendCmd[F]] {
    override protected def requiredPermissions: Set[InvoicePermissions] = Set(InvoicePermissions.Send)

    override protected def allowedStatuses: Set[InvoiceStatus] = Set(InvoiceStatus.ReadyToSend)

    override protected def action(entity: Invoice, cmd: SendCmd[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(entity)
        .setStatus(InvoiceStatus.Sent)
        .updateLastEdited(cmd)
        .build()
  }

}
