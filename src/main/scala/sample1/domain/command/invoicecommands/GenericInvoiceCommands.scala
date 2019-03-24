package sample1.domain.command.invoicecommands

import sample1.domain.ActionStatus
import sample1.domain.command._
import sample1.domain.entity.EntityRepo
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.permissions._
import sample1.domain.user.UserId


/**
  * Generic Invoice commands
  */

trait InvoiceCreateCommand[F[_]]
  extends EntityCreateCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, InvoiceUserPermissions, InvoiceAction] {
  def create(permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice]

  override def extractActionStatuses(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    InvoiceAlgebra.actionStatuses(invoice, permissions)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] =
    input.invoiceRepo

  override def checkMinimumPermissions(permissions: InvoiceUserPermissions): Either[InvoiceError, Unit] =
    Either.cond(permissions.hasCreatePermission, (), InvoiceError.InsufficientPermissions())
}

trait InvoiceQueryCommand[F[_], R]
  extends EntityQueryCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, R, InvoiceRepo[F], InvoiceUserPermissions, InvoiceAction] {
  override def extractRepo(input: DomainCommandInput[F]): InvoiceRepo[F] = input.invoiceRepo

  override def extractActionStatuses(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    InvoiceAlgebra.actionStatuses(invoice, permissions)

  override def checkMinimumPermissions(permissions: InvoiceUserPermissions): Either[InvoiceError, Unit] =
    Either.cond(permissions.hasReadPermission, (), InvoiceError.InsufficientPermissions())
}

trait InvoiceUpdateCommand[F[_], CmdType, ActionType <: InvoiceAction]
  extends EntityUpdateCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, InvoiceUserPermissions, InvoiceAction, ActionType] {
  override def action(invoice: Invoice, permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice]

  override def staleF(id: InvoiceId): InvoiceError = InvoiceError.StaleInvoiceError(id)

  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] =
    input.invoiceRepo

  override def extractActionStatuses(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    InvoiceAlgebra.actionStatuses(invoice, permissions)

  override def checkMinimumPermissions(permissions: InvoiceUserPermissions): Either[InvoiceError, Unit] =
    Either.cond(permissions.hasReadPermission, (), InvoiceError.InsufficientPermissions())
}

final case class InvoiceRetrieveCommand[F[_]](userId: UserId,
                                              id: InvoiceId,
                                              permissionsRetriever: InvoiceEntityPermissionRetriever[F])
  extends EntityRetrieveCommand[F, DomainCommandInput[F], InvoiceError, InvoiceId, Invoice, InvoiceUserPermissions, InvoiceAction] {
  override def extractRepo(input: DomainCommandInput[F]): EntityRepo[F, InvoiceId, Invoice, InvoiceError] =
    input.invoiceRepo

  override def extractActionStatuses(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    InvoiceAlgebra.actionStatuses(invoice, permissions)

  override def checkMinimumPermissions(permissions: InvoiceUserPermissions): Either[InvoiceError, Unit] =
    Either.cond(permissions.hasReadPermission, (), InvoiceError.InsufficientPermissions())
}
