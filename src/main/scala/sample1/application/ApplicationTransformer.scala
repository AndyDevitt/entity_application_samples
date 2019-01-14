package sample1.application

import cats.syntax.either._
import sample1.domain.Decoder

trait ApplicationTransformer[A, B, E] extends Decoder[A, B, E]

object ApplicationTransformer {
  def instance[A, B, E](f: B => Either[E, A]): ApplicationTransformer[A, B, E] = (b: B) => f(b)

  // Instances
  implicit def default[A, E]: ApplicationTransformer[A, A, E] = ApplicationTransformer.instance(a => a.asRight[E])

//  implicit def seq[A, B, E](implicit inner: ApplicationTransformer[A, B, E]): ApplicationTransformer[Seq[A], Seq[B], E] =
//    ApplicationTransformer.instance(bs => bs.map(inner.decode).collectFirst({ case Left(e) => e }).toLeft(bs.collect { case Right(r) => r.asInstanceOf[A] }))
}
