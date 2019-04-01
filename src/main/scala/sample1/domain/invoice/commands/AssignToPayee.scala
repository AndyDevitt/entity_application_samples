package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.{EntityVersion, RetrieveActionStatus}
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceStatus.Draft
import sample1.domain.invoice._
import sample1.domain.permissions.{EntityPermissionsRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId

object AssignToPayee {

  final case class AssignToPayeeCmd[F[_]](userId: UserId,
                                          id: InvoiceId,
                                          version: EntityVersion,
                                          permissionsRetriever: EntityPermissionsRetriever[F, InvoiceId, Invoice, InvoiceUserPermissions])
    extends InvoiceUpdateCommand[F, InvoiceAction.AssignToPayee.type] {
    override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      AssignToPayeeCmdProcessor().process(invoice, this, permissions)
  }

  implicit val assingToPayeeResolver: RetrieveActionStatus[InvoiceAction.AssignToPayee.type, Invoice, InvoiceUserPermissions] =
    (action: InvoiceAction.AssignToPayee.type, entity: Invoice, permissions: InvoiceUserPermissions) =>
      (action, AssignToPayee.AssignToPayeeCmdProcessor().actionStatus(entity, action, permissions))

  final case class AssignToPayeeCmdProcessor[F[_]]()
    extends InvoiceCommandProcessor[F, InvoiceAction.AssignToPayee.type, AssignToPayeeCmd[F]] {

    override val allowedStatuses: Set[InvoiceStatus] = Set(Draft)
    override val requiredPermissions: Set[InvoicePermissions] = Set(InvoicePermissions.AssignToPayee)

    override protected def action(entity: Invoice, cmd: AssignToPayeeCmd[F], permissions: InvoiceUserPermissions
                                 ): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(entity)
        .setStatus(InvoiceStatus.Assigned)
        .updateLastEdited(cmd)
        .build()
  }

}
