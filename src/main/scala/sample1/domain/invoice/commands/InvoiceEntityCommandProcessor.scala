package sample1.domain.invoice.commands

import cats.syntax.either._
import sample1.domain.command.EntityUpdateCommand
import sample1.domain.entity.EntityCommandProcessor
import sample1.domain.errors.InvoiceError
import sample1.domain.invoice._
import sample1.domain.permissions.{InvoicePermissions, InvoiceUserPermissions}
import sample1.domain.{ActionStatus, NotAllowed}

sealed trait InvoiceEntityCommandProcessor[F[_], EntSubType <: Invoice, ActionType <: InvoiceAction, CmdType <: EntityUpdateCommand[F, _, InvoiceError, _, Invoice, InvoiceUserPermissions, InvoiceAction, ActionType]]
  extends EntityCommandProcessor[F, Invoice, EntSubType, InvoiceError, InvoicePermissions, InvoiceUserPermissions, InvoiceAction, ActionType, CmdType, ActionStatus, NotAllowed, InvoiceStatus] {

  override def statusToErrF: NotAllowed => InvoiceError = InvoiceError.fromActionStatus

  override def staleF: Invoice => InvoiceError = i => InvoiceError.StaleInvoiceError(i.id)

  override protected def minimumAccessPermissionsCheck(entity: Invoice, permissions: InvoiceUserPermissions): Either[NotAllowed, Unit] =
    InvoiceAlgebra.minimumAccessPermissionsCheck(entity, permissions).leftMap(_ => ActionStatus.AccessDenied())
}

trait InvoiceCommandProcessor[F[_], ActionType <: InvoiceAction, CmdType <: EntityUpdateCommand[F, _, InvoiceError, _, Invoice, InvoiceUserPermissions, InvoiceAction, ActionType]]
  extends InvoiceEntityCommandProcessor[F, Invoice, ActionType, CmdType] {
  override protected def downCaster(entity: Invoice): Either[NotAllowed, Invoice] = entity.asRight[NotAllowed]
}

trait SiteInvoiceCommandProcessor[F[_], ActionType <: InvoiceAction, CmdType <: EntityUpdateCommand[F, _, InvoiceError, _, Invoice, InvoiceUserPermissions, InvoiceAction, ActionType]]
  extends InvoiceEntityCommandProcessor[F, SiteInvoice, ActionType, CmdType] {
  override protected def downCaster(entity: Invoice): Either[NotAllowed, SiteInvoice] = entity match {
    case i: SiteInvoice => i.asRight[NotAllowed]
    case _ => ActionStatus.NotAllowedForProcessType().asLeft[SiteInvoice]
  }
}

trait SponsorInvoiceCommandProcessor[F[_], ActionType <: InvoiceAction, CmdType <: EntityUpdateCommand[F, _, InvoiceError, _, Invoice, InvoiceUserPermissions, InvoiceAction, ActionType]]
  extends InvoiceEntityCommandProcessor[F, SponsorInvoice, ActionType, CmdType] {
  override protected def downCaster(entity: Invoice): Either[NotAllowed, SponsorInvoice] = entity match {
    case i: SponsorInvoice => i.asRight[NotAllowed]
    case _ => ActionStatus.NotAllowedForProcessType().asLeft[SponsorInvoice]
  }
}
