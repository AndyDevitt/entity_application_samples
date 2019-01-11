package sample1.domain.entity

import cats.syntax.either._
import sample1.domain.Codec

trait EntityRepoCodec[A, B, E] extends Codec[A, B, E]

object EntityRepoCodec {
  def instance[A, B, E](encodeF: A => B, decodeF: B => Either[E, A]): EntityRepoCodec[A, B, E] =
    new EntityRepoCodec[A, B, E] {
      override def encode(a: A): B = encodeF(a)

      override def decode(b: B): Either[E, A] = decodeF(b)
    }

  // Default instances
  implicit def sameTypeCodec[A, E]: EntityRepoCodec[A, A, E] = EntityRepoCodec.instance((a: A) => a, (b: A) => b.asRight[E])
}
