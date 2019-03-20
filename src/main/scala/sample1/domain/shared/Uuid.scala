package sample1.domain.shared

import scala.util.Random

case class Uuid(value: Int)

object Uuid {
  def apply(): Uuid = new Uuid(Random.nextInt().abs)
}
