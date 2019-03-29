package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceStatus.{Approved, NotApproved}
import sample1.domain.invoice._
import sample1.domain.permissions.{InvoiceEntityPermissionRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId
import sample1.domain.{ActionStatus, NotAllowed}

object UpdateRfi {

  final case class UpdateRfiCmd[F[_]](userId: UserId,
                                      id: InvoiceId,
                                      version: EntityVersion,
                                      permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                     ) extends InvoiceUpdateCommand[F, UpdateRfiCmd[F], InvoiceAction.UpdateRfi.type] {
    override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      UpdateRfiCmdProcessor().process(invoice, this, permissions)

    override def associatedAction: InvoiceAction.UpdateRfi.type = InvoiceAction.UpdateRfi
  }

  final case class UpdateRfiCmdProcessor[F[_]]()
    extends InvoiceEntityCommandProcessor[F, Invoice, InvoiceAction.UpdateRfi.type, UpdateRfiCmd[F]] {
    override def canDo(entity: Invoice,
                       action: InvoiceAction.UpdateRfi.type,
                       permissions: InvoiceUserPermissions
                      ): Either[NotAllowed, Invoice] = entity match {
      case si: SponsorInvoice if Set(NotApproved).exists(_ == si.status) => Right(si)
      case _: SponsorInvoice => Left(ActionStatus.NotAllowedInCurrentStatus())
      case _: SiteInvoice => Left(ActionStatus.NotAllowedForProcessType())
    }

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
