package sample1.domain.user

final case class UserId(id: String)

object UserId {
  def apply(): UserId = new UserId("User1")
}
