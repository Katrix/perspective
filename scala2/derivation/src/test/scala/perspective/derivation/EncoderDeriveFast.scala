package perspective.derivation

import cats.Id
import io.circe._
import io.circe.syntax._
import perspective._
import perspective.syntax.all._

object EncoderDeriveFast {

  implicit def productEncoder[A, Gen[_[_]]](
      implicit gen: HKDProductGeneric.Aux[A, Gen],
      encoders: Gen[Encoder]
  ): Encoder[A] = new Encoder[A] {
    private val names = gen.names

    override def apply(a: A): Json = {
      import gen.implicits._

      val list = gen.tabulateFoldLeft(Nil: List[(String, Json)])(acc =>
        Lambda[gen.Index ~>: Const[List[(String, Json)], *]] { idx =>
          val encoder = encoders.indexKC(idx)
          val name = names.indexKC(idx)
          val value = gen.productElementId(a, idx)

          (name, encoder(value)) :: acc
        }
      )

      Json.obj(list: _*)
    }
  }

  def deriver[A] = new EncoderDeriver[A]

  class EncoderDeriver[A] {
    def deriveProduct[Gen[_[_]]](
        implicit gen: HKDProductGeneric.Aux[A, Gen],
        encoders: Gen[Encoder]
    ): Encoder[A] = productEncoder
  }
}
