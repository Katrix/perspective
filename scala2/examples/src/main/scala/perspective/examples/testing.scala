package perspective.examples

import cats.Id
import cats.instances.either._
import perspective._
import perspective.derivation._
import perspective.syntax.all._

trait ACursor {
  def get[A: Decoder](field: String): Either[String, A]
}

trait Decoder[A] {
  def decode(cursor: ACursor): Either[String, A]
}
object Decoder {
  implicit val decodeInt: Decoder[Int]       = ???
  implicit val decodeString: Decoder[String] = ???

  implicit def productDecoder[A, Gen[_[_]]](
      implicit gen: HKDProductGeneric.Aux[A, Gen],
      decoders: Gen[Decoder]
  ): Decoder[A] = new Decoder[A] {
    override def decode(cursor: ACursor): Either[String, A] = {
      import gen.implicits._

      gen.names
        .map2KC(decoders)(
          Lambda[Tuple2K[Const[String, *], Decoder, *] ~>: Either[String, *]](t => cursor.get(t._1)(t._2))
        )
        .sequenceIdKC
        .map(gen.from)
    }
  }

  implicit def sumDecoder[A, Gen[_[_]]](
      implicit gen: HKDSumGeneric.Aux[A, Gen],
      decoders: Gen[Decoder]
  ): Decoder[A] = new Decoder[A] {
    override def decode(cursor: ACursor): Either[String, A] = {
      import gen.implicits._

      for {
        typeName <- cursor.get[String]("$type")
        index    <- gen.indexNameMap.get(typeName).toRight(s"$typeName is not a valid ${gen.typeName}")
        decoder = decoders.indexKC(index)
        res <- decoder.decode(cursor)
      } yield res
    }
  }
}

trait Json

trait Encoder[A] {
  def encode(a: A): Json
}
object Encoder {
  implicit val encodeInt: Encoder[Int]                   = ???
  implicit val encodeString: Encoder[String]             = ???
  implicit val encodeJsonMap: Encoder[Map[String, Json]] = ???

  implicit def productEncoder[A, Gen[_[_]]](
      implicit gen: HKDProductGeneric.Aux[A, Gen],
      encoders: Gen[Encoder]
  ): Encoder[A] = new Encoder[A] {
    override def encode(a: A): Json = {
      import gen.implicits._

      val list: List[(String, Json)] =
        gen
          .to(a)
          .map2KC(encoders)(Lambda[Tuple2K[Id, Encoder, *] ~>: Const[Json, *]](t => t._2.encode(t._1)))
          .map2KC(gen.names)(
            Lambda[Tuple2K[Const[Json, *], Const[String, *], *] ~>: Const[(String, Json), *]](_.swap)
          )
          .toListKC

      implicitly[Encoder[Map[String, Json]]].encode(list.toMap)
    }
  }

  implicit def sumEncoder[A, Gen[_[_]]](
      implicit gen: HKDSumGeneric.Aux[A, Gen],
      encoders: Gen[Encoder]
  ): Encoder[A] = new Encoder[A] {
    override def encode(a: A): Json = {
      import gen.implicits._

      val encodings = gen
        .to(a)
        .map2KC(encoders)(
          λ[Tuple2K[Option, Encoder, *] ~>: Compose2[Option, Const[Json, *], *]](t => t._1.map(x => t._2.encode(x)))
        )

      encodings.indexKC(gen.indexOf(a)).get
    }
  }
}

case class Foo(i: Int, s: String)
case class Bar(
    i1_1: Int,
    i1_2: Int,
    i1_3: Int,
    i1_4: Int,
    i1_5: Int,
    i1_6: Int,
    i1_7: Int,
    i1_8: Int,
    i1_9: Int,
    i1_10: Int,
    i1_11: Int,
    i1_12: Int,
    i1_13: Int,
    i1_14: Int,
    i1_15: Int,
    i1_16: Int,
    i1_17: Int,
    i1_18: Int,
    i1_19: Int,
    i1_20: Int,
    i1_21: Int,
    i1_22: Int
)
object Foo {

  implicitly[Decoder[Foo]]
  implicitly[Encoder[Foo]]

  //implicitly[Decoder[Bar]]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
}
