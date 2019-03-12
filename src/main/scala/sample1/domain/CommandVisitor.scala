package sample1.domain

import sample1.domain.command._
import sample1.domain.invoice.InvoiceAction

trait CommandVisitor[A] {

  def visit[F[_]](cmd: ApproveCmdG[F]): A

  def visit[F[_]](cmd: CreateRfiInvoiceCmdG[F]): A

  def visit[F[_]](cmd: UpdateRfiCmdG[F]): A

}

trait ActionVisitor[A] {
  def visit(action: InvoiceAction.Approve.type): A

  def visit(action: InvoiceAction.CreateRfi.type): A

  def visit(action: InvoiceAction.UpdateRfi.type): A
}

class CommandToActionVisitor extends CommandVisitor[InvoiceAction] {
  override def visit[F[_]](cmd: ApproveCmdG[F]): InvoiceAction.Approve.type = InvoiceAction.Approve

  override def visit[F[_]](cmd: CreateRfiInvoiceCmdG[F]): InvoiceAction.CreateRfi.type = InvoiceAction.CreateRfi

  override def visit[F[_]](cmd: UpdateRfiCmdG[F]): InvoiceAction.UpdateRfi.type = InvoiceAction.UpdateRfi
}

class PrettifyActionVisitor extends ActionVisitor[String] {
  override def visit(action: InvoiceAction.Approve.type): String = s"`${action.toString}`"

  override def visit(action: InvoiceAction.CreateRfi.type): String = s"`${action.toString}`"

  override def visit(action: InvoiceAction.UpdateRfi.type): String = s"`${action.toString}`"
}

object CommandVisitor {
  def processCommand[A](cmd: Command, visitor: CommandVisitor[A]): A = cmd match {
    case c: ApproveCmdG[_] => visitor.visit(c)
    case c: CreateRfiInvoiceCmdG[_] => visitor.visit(c)
    case c: UpdateRfiCmdG[_] => visitor.visit(c)
  }
}

object ActionVisitor {
  def processAction[A](action: InvoiceAction, visitor: ActionVisitor[A]): A = action match {
    case a: InvoiceAction.Approve.type => visitor.visit(a)
    case a: InvoiceAction.CreateRfi.type => visitor.visit(a)
    case a: InvoiceAction.UpdateRfi.type => visitor.visit(a)
  }
}

object Test extends App {
  val res = CommandVisitor.processCommand(CreateRfiInvoiceCmdG(UserId()), new CommandToActionVisitor)
  val res2 = ActionVisitor.processAction(res, new PrettifyActionVisitor)
  println(res)
  println(res2)
}
