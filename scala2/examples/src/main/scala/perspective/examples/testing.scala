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
object Decoder extends LowPriorityDecoder {
  implicit val decodeJson: Decoder[Json]                 = ???
  implicit val decodeInt: Decoder[Int]                   = ???
  implicit val decodeLong: Decoder[Long]                 = ???
  implicit val decodeDouble: Decoder[Double]             = ???
  implicit val decodeString: Decoder[String]             = ???
  implicit val decodeBoolean: Decoder[Boolean]           = ???
  implicit val decodeJsonMap: Decoder[Map[String, Json]] = ???

  implicit def decodeOption[A: Decoder]: Decoder[Option[A]] = ???
  implicit def decodeSeq[A: Decoder]: Decoder[Seq[A]]       = ???
}
sealed trait LowPriorityDecoder {

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

  def deriver[A] = new DecoderDeriver[A]

  class DecoderDeriver[A] {
    def deriveProduct[Gen[_[_]]](
        implicit gen: HKDProductGeneric.Aux[A, Gen],
        decoders: Gen[Decoder]
    ): Decoder[A] = productDecoder
  }

  implicit def sumDecoder[A, Gen[_[_]]](
      implicit gen: HKDSumGeneric.Aux[A, Gen],
      decoders: Gen[Decoder]
  ): Decoder[A] = new Decoder[A] {
    override def decode(cursor: ACursor): Either[String, A] = {
      import gen.implicits._

      for {
        typeName <- cursor.get[String]("$type")
        index    <- gen.nameToIndexMap.get(typeName).toRight(s"$typeName is not a valid ${gen.typeName}")
        decoder = decoders.indexKC(index)
        res <- decoder.decode(cursor.get[Json]("$value").fold(_ => cursor, _.cursor))
      } yield res
    }
  }
}

trait Json {
  def fields: Option[Map[String, Json]]
  def cursor: ACursor
}

trait Encoder[A] {
  def encode(a: A): Json
}
object Encoder extends LowPriorityEncoder {
  implicit val encodeInt: Encoder[Int]                   = ???
  implicit val encodeString: Encoder[String]             = ???
  implicit val encodeDouble: Encoder[Double]             = ???
  implicit val encodeLong: Encoder[Long]                 = ???
  implicit val encodeBoolean: Encoder[Boolean]           = ???
  implicit val encodeJsonMap: Encoder[Map[String, Json]] = ???

  implicit def encodeOption[A: Encoder]: Encoder[Option[A]] = ???
  implicit def encodeSeq[A: Encoder]: Encoder[Seq[A]]       = ???
}
sealed trait LowPriorityEncoder {

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
          .map2KC(gen.names)(FunctionK.liftConst((t: (Json, String)) => t.swap))
          .toListKC

      Encoder.encodeJsonMap.encode(list.toMap)
    }
  }

  def deriver[A] = new EncoderDeriver[A]

  class EncoderDeriver[A] {
    def deriveProduct[Gen[_[_]]](
        implicit gen: HKDProductGeneric.Aux[A, Gen],
        encoders: Gen[Encoder]
    ): Encoder[A] = productEncoder
  }

  implicit def sumEncoder[A, Gen[_[_]]](
      implicit gen: HKDSumGeneric.Aux[A, Gen],
      encoders: Gen[Encoder]
  ): Encoder[A] = new Encoder[A] {
    override def encode(a: A): Json = {
      import gen.implicits._

      val typeName = gen.nameToIndexMap.map(_.swap)(gen.indexOf(a))

      val encodings = gen
        .to(a)
        .map2KC(encoders)(
          λ[Tuple2K[Option, Encoder, *] ~>: Compose2[Option, Const[Json, *], *]](t => t._1.map(x => t._2.encode(x)))
        )

      val json = encodings.indexKC(gen.indexOf(a)).get
      json.fields match {
        case Some(fields) =>
          Encoder.encodeJsonMap.encode(fields.updated("$type", Encoder.encodeString.encode(typeName)))
        case None =>
          Encoder.encodeJsonMap.encode(Map("$type" -> Encoder.encodeString.encode(typeName), "$value" -> json))
      }
    }
  }
}

trait Codec[A] extends Encoder[A] with Decoder[A]
object Codec {

  implicit def createCodec[A](implicit encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = new Codec[A] {
    override def decode(cursor: ACursor): Either[String, A] = decoder.decode(cursor)

    override def encode(a: A): Json = encoder.encode(a)
  }

  def deriver[A] = new CodecDeriver[A]

  class CodecDeriver[A] {
    def derive[Gen[_[_]]](
        implicit gen: HKDProductGeneric.Aux[A, Gen],
        encoders: Gen[Encoder],
        deceoders: Gen[Decoder]
    ): Codec[A] = createCodec(Encoder.productEncoder, Decoder.productDecoder)
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
  //implicitly[Decoder[Foo]]
  //implicitly[Encoder[Foo]]

  //implicitly[Encoder[(String, Int)]]

  //implicitly[Decoder[Bar]]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
}

sealed trait SumTest
object SumTest {
  case class Foo(a: String, b: Int)    extends SumTest
  case class Bar(c: Double, b: String) extends SumTest

  HKDSumGeneric.materializeHKDSum[SumTest]
}

sealed trait Sum23Test
object Sum23Test {
  case class S1(a: String)  extends Sum23Test
  case class S2(a: String)  extends Sum23Test
  case class S3(a: String)  extends Sum23Test
  case class S4(a: String)  extends Sum23Test
  case class S5(a: String)  extends Sum23Test
  case class S6(a: String)  extends Sum23Test
  case class S7(a: String)  extends Sum23Test
  case class S8(a: String)  extends Sum23Test
  case class S9(a: String)  extends Sum23Test
  case class S10(a: String) extends Sum23Test
  case class S11(a: String) extends Sum23Test
  case class S12(a: String) extends Sum23Test
  case class S13(a: String) extends Sum23Test
  case class S14(a: String) extends Sum23Test
  case class S15(a: String) extends Sum23Test
  case class S16(a: String) extends Sum23Test
  case class S17(a: String) extends Sum23Test
  case class S18(a: String) extends Sum23Test
  case class S19(a: String) extends Sum23Test
  case class S20(a: String) extends Sum23Test
  case class S21(a: String) extends Sum23Test
  case class S22(a: String) extends Sum23Test
  case class S23(a: String) extends Sum23Test

  HKDSumGeneric.materializeHKDSum[Sum23Test]
}
