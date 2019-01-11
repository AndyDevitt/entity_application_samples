package sample1.application

import cats.syntax.either._
import sample1.domain.Decoder

trait ApplicationTransformer[A, B, E] extends Decoder[A, B, E]

object ApplicationTransformer {
  def instance[A, B, E](f: B => Either[E, A]): ApplicationTransformer[A, B, E] = (b: B) => f(b)

  // Instances
  implicit def default[A, E]: ApplicationTransformer[A, A, E] = ApplicationTransformer.instance(a => a.asRight[E])
}
