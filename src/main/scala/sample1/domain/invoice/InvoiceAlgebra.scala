package sample1.domain.invoice

import sample1.domain._
import sample1.domain.command._
import sample1.domain.entity.{EntityId, VersionedEntity}
import sample1.domain.errors.{InvoiceError, ValidationError}
import sample1.domain.invoice.InvoiceStateBuilder.Instances._
import sample1.domain.permissions.InvoiceUserPermissions
import sample1.utils.ReduceOptionWithFailure._

trait EntityInterface[IdType <: EntityId, EntityType <: VersionedEntity[IdType], ErrType, ActionType, ActionStatusType, NotAllowedActionStatusType <: ActionStatusType, PermissionsType] {
  def staleF: EntityType => ErrType

  //  def actionFromCommand(cmd: Command): ActionType

  def statusToErrF: NotAllowedActionStatusType => ErrType

  //def actionStatus(entity: EntityType, action: ActionType): ActionStatusType

  def checkOptimisticLocking[A <: EntityType](entity: A, cmd: Command): Either[ErrType, A] = cmd match {
    case c: EntityUpdateCommand[_, _, _, _, _, _, _] if c.enforceOptimisticLocking && c.version != entity.version => Left(staleF(entity))
    case _ => Right(entity)
  }

  //  def canDoAction[A <: EntityType](f: (EntityType, PermissionsType) => Either[NotAllowedActionStatusType, A])(entity: EntityType, cmd: Command, permissions: PermissionsType): Either[ErrType, A] =
  //    f(entity, permissions)
  //      .left.map(statusToErrF)
  //      .flatMap(inv => checkOptimisticLocking(inv, cmd))

  def canDoAction2[A <: EntityType](f: (EntityType, PermissionsType) => Either[NotAllowedActionStatusType, A])(entity: EntityType, cmd: Command, permissions: PermissionsType): Either[ErrType, A] =
    f(entity, permissions)
      .left.map(statusToErrF)
      .flatMap(inv => checkOptimisticLocking(inv, cmd))

//  def actionStatus(action: ActionType): ActionStatusType
//
//  def actionStatus[F[_]](cmd: EntityUpdateCommand[F, _, _, IdType, EntityType, PermissionsType, ActionType]): ActionStatusType =
//    actionStatus(cmd.associatedAction)

}

object InvoiceAlgebra extends EntityInterface[InvoiceId, Invoice, InvoiceError, InvoiceAction, ActionStatus, NotAllowed, InvoiceUserPermissions] {

  import cats.data.State
  import sample1.domain.invoice.InvoiceUtils._

  override def staleF: Invoice => InvoiceError = i => InvoiceError.StaleInvoiceError(i.id)

  override def statusToErrF: NotAllowed => InvoiceError = i => InvoiceError.fromActionStatus(i)

  //  def actionFromCommand(cmd: Command): InvoiceAction = cmd match {
  //    case _: ApproveCmd[_] => InvoiceAction.Approve
  //    case _: CreateRfiInvoiceCmd[_] => InvoiceAction.CreateRfi
  //    case _: UpdateRfiCmd[_] => InvoiceAction.UpdateRfi
  //  }

  def actionStatuses(invoice: Invoice, permissions: InvoiceUserPermissions): Set[(InvoiceAction, ActionStatus)] =
    EnumerableAdt[InvoiceAction].map(action => (action, actionStatus(invoice, action, permissions)))

  //  def actionStatus(invoice: Invoice, cmd: Command): ActionStatus =
  //    actionStatus(invoice, actionFromCommand(cmd))

  def actionStatus(invoice: Invoice, action: InvoiceAction, permissions: InvoiceUserPermissions): ActionStatus = {
    action match {
      case a: InvoiceAction.Approve.type => canDo(invoice, a, permissions)
      case a: InvoiceAction.UpdateRfi.type => canDo(invoice, a, permissions)
    }
  }.fold[ActionStatus]((na: NotAllowed) => na, _ => Allowed)

  //  private def canDoAction[F[_], A <: Invoice, ActionType](invoice: Invoice, cmd: EntityUpdateCommand[F, _, _, _, _, _, ActionType], permissions: InvoiceUserPermissions): Either[InvoiceError, A] =
  //    canDo(invoice, permissions, cmd.associatedAction)
  //      .left.map(InvoiceError.fromActionStatus)
  //      .flatMap(inv => checkOptimisticLocking(inv, cmd))

  private def canDoWrapper[ActionType](cmd: Command)(result: Either[NotAllowed, Invoice]): Either[InvoiceError, Invoice] =
    result.left.map(InvoiceError.fromActionStatus)
      .flatMap(inv => checkOptimisticLocking(inv, cmd))


  //  private def checkOptimisticLocking[A <: Invoice](invoice: A, cmd: Command): Either[InvoiceError, A] = cmd match {
  //    case c: EntityUpdateCommand[_, _, _, _, _] if c.enforceOptimisticLocking && c.version != invoice.version => Left(StaleInvoiceError(invoice.id))
  //    case _ => Right(invoice)
  //  }

  //  def canDo(invoice: Invoice, action: InvoiceAction.CreateRfi.type, permissions: InvoiceUserPermissions): Either[NotAllowed, SponsorInvoice] = invoice match {
  //    case si: SponsorInvoice => Right(si)
  //    case _: SiteInvoice => Left(NotAllowedForProcessType())
  //  }

  //  def createRfi[F[_]](invoice: Invoice, cmd: CreateRfiInvoiceCmd[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
  //    canDo(invoice, permissions) map {
  //      InvoiceUtils.createRfi(_, cmd)
  //    }
  //
  def canDo(invoice: Invoice, action: InvoiceAction.Approve.type, permissions: InvoiceUserPermissions): Either[NotAllowed, Invoice] =
    Either.cond(invoice.status == NotApproved, invoice, NotAllowedInCurrentStatus())

  def approve[F[_]](invoice: Invoice, cmd: ApproveCmd[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    canDoWrapper(cmd)(canDo(invoice, cmd.associatedAction, permissions) map { inv =>
      val pgm = for {
        _ <- State[Invoice, Unit] { s => (clearCosts(s, cmd), ()) }
        _ <- State[Invoice, Unit] { s => (setStatus(s, cmd, Approved), ()) }
      } yield ()
      pgm.runS(inv).value
    })

  def approveV2[F[_]](invoice: Invoice, cmd: ApproveCmdV2[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    canDoWrapper(cmd)(canDo(invoice, cmd.associatedAction, permissions) map {
      InvoiceStateBuilder.Builder(_)
        .clearCosts()
        .setStatus(Approved)
        .updateLastEdited(cmd)
        .build()
    })

  def canDo(invoice: Invoice, action: InvoiceAction.UpdateRfi.type, permissions: InvoiceUserPermissions): Either[NotAllowed, SponsorInvoice] = invoice match {
    case si: SponsorInvoice if Set(NotApproved).exists(_ == si.status) => Right(si)
    case si: SponsorInvoice => Left(NotAllowedInCurrentStatus())
    case _: SiteInvoice => Left(NotAllowedForProcessType())
  }

  def updateRfi[F[_]](invoice: Invoice, cmd: UpdateRfiCmd[F], permissions: InvoiceUserPermissions): Either[InvoiceError, Invoice] =
    canDoWrapper(cmd)(canDo(invoice, cmd.associatedAction, permissions) map { inv =>
      val pgm = for {
        _ <- InvoiceStateUtils.clearCosts(inv, cmd)
        _ <- InvoiceStateUtils.setStatus(inv, cmd, Approved)
        _ <- InvoiceStateUtils.updateRfi(inv, cmd, RequestForInvoice())
      } yield ()
      pgm.runS(inv).value
    })

  def calculateTotal(invoice: Invoice): Either[ValidationError, Option[MonetaryAmount]] =
    invoice.costs
      .map(_.amount)
      .reduceOptionWithFailure(MonetaryAmountAlg.sum)

}
