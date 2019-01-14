package sample1.domain

trait Transform[From, To, Error] {
  def to(from: From): Either[Error, To]
}

object Transform {

  implicit class ops[From, Error](from: Either[Error, From]) {
    def to[To](implicit trans: Transform[From, To, Error]): Either[Error, To] = from.flatMap(trans.to)
  }

  object Instances {
    implicit def seqConverter[From, To, Error](implicit transformTo: Transform[From, To, Error]
                                              ): Transform[Seq[From], Seq[To], Error] =
      (from: Seq[From]) => from.map(transformTo.to).collectFirst({ case Left(e) => e }).toLeft(from.collect { case Right(r) => r.asInstanceOf[To] })
  }

}
