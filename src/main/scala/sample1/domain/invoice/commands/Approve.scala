package sample1.domain.invoice.commands

import cats.syntax.either._
import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceAlgebra.calculateTotal
import sample1.domain.invoice.InvoiceStatus.{Approved, NotApproved}
import sample1.domain.invoice._
import sample1.domain.permissions.{InvoiceEntityPermissionRetriever, InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.user.UserId
import sample1.domain.{ActionStatus, NotAllowed}

object Approve {

  final case class ApproveCmd[F[_]](userId: UserId,
                                    id: InvoiceId,
                                    version: EntityVersion,
                                    permissionsRetriever: InvoiceEntityPermissionRetriever[F]
                                   ) extends InvoiceUpdateCommand[F, ApproveCmd[F], InvoiceAction.Approve.type] {
    override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      ApproveCmdProcessor().process(invoice, this, permissions)
  }

  // TODO: provide this action as an example of where the command state is NOT required to check full permissions (i.e.
  //  cannot approve an invoice over a certain amount). Use this to add some common comparison behaviour to permissions
  //  possibly..
  final case class ApproveCmdProcessor[F[_]]()
    extends InvoiceCommandProcessor[F, InvoiceAction.Approve.type, ApproveCmd[F]] {

    override val allowedStatuses: Set[InvoiceStatus] = Set(NotApproved)
    override val requiredPermissions: Set[InvoicePermissions] = Set(InvoicePermissions.Read, InvoicePermissions.Approve)

    override protected def checkAdditionalRequirements(entity: Invoice,
                                                       action: InvoiceAction.Approve.type,
                                                       permissions: InvoiceUserPermissions
                                                      ): Either[NotAllowed, Invoice] =
      for {
        limit <- Either.fromOption(
          permissions.approvalLimit.map(_.limit),
          ActionStatus.NotEnoughPermissions("No approval limit found"))
        // TODO: The following statement if it failed would actually be a bug in the application - figure out how this
        //  should be handled. It feels like there should be an error, but the signature doesn't currently allow for
        //  this...
        totalOpt <- calculateTotal(entity).leftMap(_ => ActionStatus.UnknownStatus("Error calculating the total"))
        total <- Either.fromOption(totalOpt, ActionStatus.NotAllowedInCurrentState("Cannot approve with no costs"))
        _ <- Either.cond(
          limit >= total.amount,
          (),
          ActionStatus.NotEnoughPermissions(s"Approval limit is below the invoice total ($limit < ${total.amount})"))
      } yield entity

    override protected def action(entity: Invoice,
                                  cmd: ApproveCmd[F],
                                  permissions: InvoiceUserPermissions
                                 ): Either[InvoiceError, Invoice] =
      InvoiceStateBuilder.Builder(entity)
        .setStatus(Approved)
        .updateLastEdited(cmd)
        .build()
  }

}
