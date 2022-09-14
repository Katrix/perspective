package perspective.derivation

import scala.language.implicitConversions

import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.*

import cats.Id
import cats.instances.either.*
import io.circe.*
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import perspective.*

object CodecTests {
  trait PerspectiveDecoder[A] extends Decoder[A]
  object PerspectiveDecoder:

    def derivedProductDecoder[A](
        using gen: HKDProductGeneric[A],
        decoders: gen.Gen[Decoder]
    ): PerspectiveDecoder[A] = new PerspectiveDecoder[A]:
      override def apply(cursor: HCursor): Either[DecodingFailure, A] =
        import gen.given

        gen.names
          .map2K(decoders)(
            [Z] => (name: gen.Names, decoder: Decoder[Z]) => cursor.get(name)(using decoder)
          )
          .sequenceIdK
          .map(gen.from)

    def derivedSumDecoder[A](
        using gen: HKDSumGeneric[A],
        decoders: gen.Gen[Decoder]
    ): PerspectiveDecoder[A] = new PerspectiveDecoder[A]:
      override def apply(cursor: HCursor): Either[DecodingFailure, A] =
        import gen.given

        for
          typeNameStr <- cursor.get[String]("$type")
          typeName <- gen
            .stringToName(typeNameStr)
            .toRight(DecodingFailure(s"$typeNameStr is not a valid ${gen.typeName}", cursor.history))
          index       = gen.nameToIndex(typeName)
          decoder     = decoders.indexK(index)
          valueCursor = cursor.downField("$value")
          res <- decoder(cursor.downField("$value").success.getOrElse(cursor))
        yield res

    private inline def caseDecoders[XS <: Tuple]: Tuple.Map[XS, Decoder] = inline erasedValue[XS] match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: t) =>
        val headDecoder: Decoder[h] = summonFrom {
          case d: Decoder[`h`]      => d
          case gen: HKDGeneric[`h`] => derived[h]
        }
        headDecoder *: caseDecoders[t]

    inline def derived[A](using gen: HKDGeneric[A]): PerspectiveDecoder[A] = inline gen match
      case gen: HKDProductGeneric.Aux[A, gen.Gen] =>
        val decoders = summonInline[gen.Gen[Decoder]]
        derivedProductDecoder(using gen, decoders)
      case gen: HKDSumGeneric.Aux[A, gen.Gen] =>
        summonFrom {
          case decoders: gen.Gen[Decoder] => derivedSumDecoder(using gen, decoders)
          case _ =>
            val decodersTuple = caseDecoders[gen.TupleRep]
            val decoders      = gen.tupleToGen(decodersTuple)
            derivedSumDecoder(using gen, decoders)
        }

  trait PerspectiveEncoder[A] extends Encoder[A]
  object PerspectiveEncoder:

    def derivedProductEncoder[A](
        using gen: HKDProductGeneric[A],
        encoders: gen.Gen[Encoder]
    ): PerspectiveEncoder[A] = new PerspectiveEncoder[A]:
      override def apply(a: A): Json =
        import gen.given

        val list: List[(String, Json)] =
          gen
            .to(a)
            .map2Const(encoders)([Z] => (caseObj: Z, encoder: Encoder[Z]) => encoder(caseObj))
            // TODO Type needed
            .map2Const[Const[gen.Names], (gen.Names, Json)](gen.names)(
              [Z] => (json: Json, name: gen.Names) => (name, json)
            )
            .toListK

        Json.obj(list: _*)

    def derivedSumEncoder[A](
        using gen: HKDSumGeneric[A],
        encoders: gen.Gen[Encoder]
    ): PerspectiveEncoder[A] = new PerspectiveEncoder[A]:
      override def apply(a: A): Json =
        import gen.given

        val typeName = (gen.indexToName(gen.indexOfA(a)): String).asJson

        val encodings =
          gen
            .to(a)
            .map2Const(encoders)([Z] => (optCase: Option[Z], encoder: Encoder[Z]) => optCase.map(x => encoder(x)))

        val json = encodings.indexK(gen.indexOfA(a)).get
        json.asObject match
          case Some(fields) => json.deepMerge(Json.obj("$type" -> typeName))
          case None         => Json.obj("$type" -> typeName, "$value" -> json)

    private inline def caseEncoders[XS <: Tuple]: Tuple.Map[XS, Encoder] = inline erasedValue[XS] match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: t) =>
        val headEncoder: Encoder[h] = summonFrom {
          case e: Encoder[`h`]      => e
          case gen: HKDGeneric[`h`] => derived[h]
        }
        headEncoder *: caseEncoders[t]

    inline def derived[A](using gen: HKDGeneric[A]): PerspectiveEncoder[A] = inline gen match
      case gen: HKDProductGeneric.Aux[A, gen.Gen] =>
        val encoders = summonInline[gen.Gen[Encoder]]
        derivedProductEncoder(using gen, encoders)
      case gen: HKDSumGeneric.Aux[A, gen.Gen] =>
        summonFrom {
          case encoders: gen.Gen[Encoder] => derivedSumEncoder(using gen, encoders)
          case _ =>
            val encodersTuple = caseEncoders[gen.TupleRep]
            val encoders      = gen.tupleToGen(encodersTuple)
            derivedSumEncoder(using gen, encoders)
        }

  case class Foo(i: Int, s: String, foobar: Long) derives PerspectiveEncoder, PerspectiveDecoder

  enum Bar derives PerspectiveEncoder, PerspectiveDecoder {
    case A(i: Int)
    case B(s: String)
    case C(foobar: Long)
  }
}
class CodecTests extends AnyFunSuite {
  import CodecTests.*

  test("Foo as json") {
    assert(Foo(5, "bar", 9L).asJson === Json.obj("i" -> 5.asJson, "s" -> "bar".asJson, "foobar" -> 9L.asJson))
  }

  test("Foo from json") {
    assert(Json.obj("i" -> 5.asJson, "s" -> "bar".asJson, "foobar" -> 9L.asJson).as[Foo].contains(Foo(5, "bar", 9L)))
  }

  test("Bar as json") {
    assert((Bar.A(5): Bar).asJson === Json.obj("$type" := "A", "i" := 5))
    assert((Bar.B("bar"): Bar).asJson === Json.obj("$type" := "B", "s" := "bar"))
    assert((Bar.C(9L): Bar).asJson === Json.obj("$type" := "C", "foobar" := 9L))
  }

  test("Bar from json") {
    assert(Json.obj("$type" := "A", "i" := 5).as[Bar].contains(Bar.A(5)))
    assert(Json.obj("$type" := "B", "s" := "bar").as[Bar].contains(Bar.B("bar")))
    assert(Json.obj("$type" := "C", "foobar" := 9L).as[Bar].contains(Bar.C(9L)))
  }

}
