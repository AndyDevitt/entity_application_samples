package sample1.domain

import cats.syntax.either._

trait Codec[A, B, E] extends Encoder[A, B] with Decoder[A, B, E]

trait Encoder[A, B] {
  def encode(a: A): B
}

object Encoder {
  // Default instances
  implicit def sameTypeEncode[A, E]: Encoder[A, A] = (a: A) => a
}

trait Decoder[A, B, E] {
  def decode(b: B): Either[E, A]
}

object Decoder {
  // Default instances
  implicit def sameTypeDecode[A, E]: Decoder[A, A, E] = (b: A) => b.asRight[E]
}

object Codec {
  def instance[A, B, E](encodeF: A => B, decodeF: B => Either[E, A]): Codec[A, B, E] =
    new Codec[A, B, E] {
      override def encode(a: A): B = encodeF(a)

      override def decode(b: B): Either[E, A] = decodeF(b)
    }

  // Default instances
  implicit def sameTypeCodec[A, E]: Codec[A, A, E] = Codec.instance((a: A) => a, (b: A) => b.asRight[E])
}
