package sample1.domain

final case class UserId(id: String)

object UserId {
  def apply(): UserId = new UserId("User1")
}
