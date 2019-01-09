package sample1.domain

import sample1.domain.command.{ApproveCmd, Command, CreateRfiInvoiceCmd, UpdateRfiCmd}
import sample1.domain.invoice.InvoiceAction

trait CommandVisitor[A] {

  def visit(cmd: ApproveCmd): A

  def visit(cmd: CreateRfiInvoiceCmd): A

  def visit(cmd: UpdateRfiCmd): A

}

trait ActionVisitor[A] {
  def visit(action: InvoiceAction.Approve.type): A

  def visit(action: InvoiceAction.CreateRfi.type): A

  def visit(action: InvoiceAction.UpdateRfi.type): A
}

class CommandToActionVisitor extends CommandVisitor[InvoiceAction] {
  override def visit(cmd: ApproveCmd): InvoiceAction.Approve.type = InvoiceAction.Approve

  override def visit(cmd: CreateRfiInvoiceCmd): InvoiceAction.CreateRfi.type = InvoiceAction.CreateRfi

  override def visit(cmd: UpdateRfiCmd): InvoiceAction.UpdateRfi.type = InvoiceAction.UpdateRfi
}

class PrettifyActionVisitor extends ActionVisitor[String] {
  override def visit(action: InvoiceAction.Approve.type): String = s"`${action.toString}`"

  override def visit(action: InvoiceAction.CreateRfi.type): String = s"`${action.toString}`"

  override def visit(action: InvoiceAction.UpdateRfi.type): String = s"`${action.toString}`"
}

object CommandVisitor {
  def processCommand[A](cmd: Command, visitor: CommandVisitor[A]): A = cmd match {
    case c: ApproveCmd => visitor.visit(c)
    case c: CreateRfiInvoiceCmd => visitor.visit(c)
    case c: UpdateRfiCmd => visitor.visit(c)
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
  val res = CommandVisitor.processCommand(CreateRfiInvoiceCmd(UserId()), new CommandToActionVisitor)
  val res2 = ActionVisitor.processAction(res, new PrettifyActionVisitor)
  println(res)
  println(res2)
}
