package sample1.domain.entity

trait CommandAction[CmdType] {
  type ActionType

  def action: ActionType
}
