package sample1.domain.shared

case class DateTime(value: String)

object DateTime {
  def now(): DateTime = new DateTime("now")
}
