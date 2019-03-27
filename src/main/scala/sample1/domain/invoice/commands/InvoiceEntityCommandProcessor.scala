package sample1.domain.invoice.commands

import cats.syntax.either._
import sample1.domain.command.EntityUpdateCommand
import sample1.domain.entity.EntityCommandProcessor
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice.{Invoice, InvoiceAction, InvoiceAlgebra}
import sample1.domain.permissions.InvoiceUserPermissions
import sample1.domain.{ActionStatus, NotAllowed}

trait InvoiceEntityCommandProcessor[F[_], EntSubType <: Invoice, ActionType <: InvoiceAction, CmdType <: EntityUpdateCommand[F, _, InvoiceError, _, Invoice, InvoiceUserPermissions, InvoiceAction, ActionType]]
  extends EntityCommandProcessor[F, Invoice, EntSubType, InvoiceError, InvoiceUserPermissions, InvoiceAction, ActionType, CmdType, ActionStatus, NotAllowed] {

  override def statusToErrF: NotAllowed => InvoiceError = InvoiceError.fromActionStatus

  override def staleF: Invoice => InvoiceError = i => InvoiceError.StaleInvoiceError(i.id)

  override protected def minimumAccessPermissionsCheck(entity: Invoice, permissions: InvoiceUserPermissions): Either[NotAllowed, Unit] =
    InvoiceAlgebra.minimumAccessPermissionsCheck(entity, permissions).leftMap(_ => ActionStatus.AccessDenied())
}
