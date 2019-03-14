package sample1.domain

import sample1.domain.command._
import sample1.domain.invoice.InvoiceAction
import sample1.domain.user.UserId

trait CommandVisitor[A] {

  def visit[F[_], H[_]](cmd: ApproveCmd[F, H]): A

  def visit[F[_], H[_]](cmd: CreateRfiInvoiceCmd[F, H]): A

  def visit[F[_], H[_]](cmd: UpdateRfiCmd[F, H]): A

}

trait ActionVisitor[A] {
  def visit(action: InvoiceAction.Approve.type): A

  def visit(action: InvoiceAction.CreateRfi.type): A

  def visit(action: InvoiceAction.UpdateRfi.type): A
}

class CommandToActionVisitor extends CommandVisitor[InvoiceAction] {
  override def visit[F[_], H[_]](cmd: ApproveCmd[F, H]): InvoiceAction.Approve.type = InvoiceAction.Approve

  override def visit[F[_], H[_]](cmd: CreateRfiInvoiceCmd[F, H]): InvoiceAction.CreateRfi.type = InvoiceAction.CreateRfi

  override def visit[F[_], H[_]](cmd: UpdateRfiCmd[F, H]): InvoiceAction.UpdateRfi.type = InvoiceAction.UpdateRfi
}

class PrettifyActionVisitor extends ActionVisitor[String] {
  override def visit(action: InvoiceAction.Approve.type): String = s"`${action.toString}`"

  override def visit(action: InvoiceAction.CreateRfi.type): String = s"`${action.toString}`"

  override def visit(action: InvoiceAction.UpdateRfi.type): String = s"`${action.toString}`"
}

object CommandVisitor {
  def processCommand[A](cmd: Command, visitor: CommandVisitor[A]): A = cmd match {
    case c: ApproveCmd[_, _] => visitor.visit(c)
    case c: CreateRfiInvoiceCmd[_, _] => visitor.visit(c)
    case c: UpdateRfiCmd[_, _] => visitor.visit(c)
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
