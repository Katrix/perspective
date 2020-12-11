package perspective.examples

import cats.Id
import perspective._
import perspective.derivation._
import perspective.syntax.all._
import io.circe._
import io.circe.syntax._

object EncoderDerive {

  implicit def productEncoder[A, Gen[_[_]]](
      implicit gen: HKDProductGeneric.Aux[A, Gen],
      encoders: Gen[Encoder]
  ): Encoder[A] = new Encoder[A] {
    override def apply(a: A): Json = {
      import gen.implicits._

      val list: List[(String, Json)] =
        gen
          .to(a)
          .map2KC(encoders)(Lambda[Tuple2K[Id, Encoder, *] ~>: Const[Json, *]](t => t._2.apply(t._1)))
          .map2KC(gen.names)(FunctionK.liftConst((t: (Json, String)) => t.swap))
          .toListKC

      Json.obj(list: _*)
    }
  }

  def deriver[A] = new EncoderDeriver[A]

  class EncoderDeriver[A] {
    def deriveProduct[Gen[_[_]]](
        implicit gen: HKDProductGeneric.Aux[A, Gen],
        encoders: Gen[Encoder]
    ): Encoder[A] = productEncoder

    def deriveSum[Gen[_[_]]](
        implicit gen: HKDSumGeneric.Aux[A, Gen],
        encoders: Gen[Encoder]
    ): Encoder[A] = sumEncoder
  }

  implicit def sumEncoder[A, Gen[_[_]]](
      implicit gen: HKDSumGeneric.Aux[A, Gen],
      encoders: Gen[Encoder]
  ): Encoder[A] = new Encoder[A] {
    override def apply(a: A): Json = {
      import gen.implicits._

      val typeName = gen.indexToNameMap(gen.indexOf(a))

      val encodings = gen
        .to(a)
        .map2KC(encoders)(
          λ[Tuple2K[Option, Encoder, *] ~>: Compose2[Option, Const[Json, *], *]](t => t._1.map(x => t._2.apply(x)))
        )

      val json: Json = encodings.indexKC(gen.indexOf(a)).get
      json.asObject match {
        case Some(jsObj) => Json.fromJsonObject(jsObj.add("$type", typeName.asJson))
        case None        => Json.obj("$type" := typeName, "$value" := json)
      }
    }
  }
}
