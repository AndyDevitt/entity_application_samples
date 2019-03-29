package sample1.domain.invoice.commands

import sample1.domain.command.invoicecommands.InvoiceUpdateCommand
import sample1.domain.entity.EntityVersion
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.InvoiceStateBuilder._
import sample1.domain.invoice.InvoiceStatus.Assigned
import sample1.domain.invoice._
import sample1.domain.permissions.{EntityPermissionsRetriever, InvoiceUserPermissions}
import sample1.domain.user.UserId
import sample1.domain.{ActionStatus, NotAllowed}

object Withdraw {

  final case class WithdrawCmd[F[_]](userId: UserId,
                                     id: InvoiceId,
                                     version: EntityVersion,
                                     permissionsRetriever: EntityPermissionsRetriever[F, InvoiceId, Invoice, InvoiceUserPermissions])
    extends InvoiceUpdateCommand[F, WithdrawCmd[F], InvoiceAction.Withdraw.type] {
    override val associatedAction: InvoiceAction.Withdraw.type = InvoiceAction.Withdraw

    override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
      WithdrawCmdProcessor().process(invoice, this, permissions)
  }

  final case class WithdrawCmdProcessor[F[_]]()
    extends InvoiceEntityCommandProcessor[F, Invoice, InvoiceAction.Withdraw.type, WithdrawCmd[F]] {

    private val allowedStatuses: Set[InvoiceStatus] = Set(Assigned)

    override protected def canDo(entity: Invoice,
                                 action: InvoiceAction.Withdraw.type,
                                 permissions: InvoiceUserPermissions
                                ): Either[NotAllowed, Invoice] =
      for {
        _ <- Either.cond(allowedStatuses.contains(entity.status), (), ActionStatus.NotAllowedInCurrentStatus())
        _ <- Either.cond(entity.costs.isEmpty, (), ActionStatus.NotAllowedInCurrentState("Cannot withdraw with associated costs"))
      } yield entity

    override protected def action(entity: Invoice,
                                  cmd: WithdrawCmd[F],
                                  permissions: InvoiceUserPermissions
                                 ): Either[InvoiceError, Invoice] =
      Builder(entity)
        .setStatus(InvoiceStatus.Draft)
        .updateLastEdited(cmd)
        .build()
  }

}
