package sample1.domain.command

import sample1.domain.UserId
import sample1.domain.entity.{EntityId, EntityVersion}
import sample1.domain.invoice.InvoiceId

trait Command {
  def userId: UserId
}

sealed trait EntityCommand extends Command

sealed trait OptimisticLocking {
  def enforceOptimisticLocking: Boolean = true
}

trait IgnoreOptimisticLocking extends OptimisticLocking {
  override def enforceOptimisticLocking: Boolean = false
}

sealed trait EntityUpdateCommand[A <: EntityId] extends EntityCommand with OptimisticLocking {
  def id: A

  def version: EntityVersion
}

final case class ApproveCmd(userId: UserId, id: InvoiceId, version: EntityVersion) extends EntityUpdateCommand[InvoiceId]

final case class CreateRfiInvoiceCmd(userId: UserId) extends EntityCommand

final case class UpdateRfiCmd(userId: UserId, id: InvoiceId, version: EntityVersion) extends EntityUpdateCommand[InvoiceId]
