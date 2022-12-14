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

object InlineCodecTests {

  trait PerspectiveInlineDecoder[A] extends Decoder[A]
  object PerspectiveInlineDecoder:

    inline def derivedProductDecoder[A](using gen: InlineHKDProductGeneric[A]): PerspectiveInlineDecoder[A] =
      new PerspectiveInlineDecoder[A]:
        private val names    = gen.names
        private val decoders = gen.summonInstances[Decoder]

        override def apply(cursor: HCursor): Either[DecodingFailure, A] =
          gen
            .tabulateTraverseIdK { i =>
              val name    = names.indexK(i)
              val decoder = decoders.indexK(i)

              cursor.get(name)(using decoder)
            }
            .map(gen.from(_))

    inline def derivedSumDecoder[A](using gen: InlineHKDSumGeneric[A]): PerspectiveInlineDecoder[A] =
      new PerspectiveInlineDecoder[A]:
        private val names = gen.names
        private val decoders = summonFrom {
          case decoders: gen.Gen[Decoder] => decoders
          case _                          => gen.tupleToGen(caseDecoders[gen.TupleRep])
        }

        override def apply(cursor: HCursor): Either[DecodingFailure, A] =
          cursor
            .get[String]("$type")
            .flatMap { typeNameStr =>
              gen
                .stringToName(typeNameStr)
                .toRight(DecodingFailure(s"$typeNameStr is not a valid ${gen.typeName}", cursor.history))
            }
            .flatMap { typeName =>
              val index   = gen.nameToIndex(typeName)
              val decoder = decoders.indexK(index)
              decoder(cursor.downField("$value").success.getOrElse(cursor))
            }

    private inline def caseDecoders[XS <: Tuple]: Tuple.Map[XS, Decoder] = inline erasedValue[XS] match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: t) =>
        val headDecoder: Decoder[h] = summonFrom {
          case d: Decoder[`h`]             => d
          case given InlineHKDGeneric[`h`] => derived[h]
        }
        headDecoder *: caseDecoders[t]

    inline def derived[A](using gen: InlineHKDGeneric[A]): PerspectiveInlineDecoder[A] = inline gen match
      case gen: InlineHKDProductGeneric.Aux[A, gen.Gen] => derivedProductDecoder(using gen)
      case gen: InlineHKDSumGeneric.Aux[A, gen.Gen]     => derivedSumDecoder(using gen)

  trait PerspectiveInlineEncoder[A] extends Encoder[A]
  object PerspectiveInlineEncoder:

    inline def derivedProductEncoder[A](using gen: InlineHKDProductGeneric[A]): PerspectiveInlineEncoder[A] =
      new PerspectiveInlineEncoder[A]:
        private val names    = gen.names
        private val encoders = gen.summonInstances[Encoder]

        override def apply(a: A): Json =
          val list = gen.tabulateFoldLeft(Nil: List[(String, Json)])((acc, idx) =>
            val field   = a.productElementId(idx)
            val name    = names.indexK(idx)
            val encoder = encoders.indexK(idx)

            (name, encoder(field)) :: acc
          )

          Json.obj(list: _*)

    inline def derivedSumEncoder[A](using gen: InlineHKDSumGeneric[A]): PerspectiveInlineEncoder[A] =
      new PerspectiveInlineEncoder[A]:
        private val encoders = summonFrom {
          case encoders: gen.Gen[Encoder] => encoders
          case _                          => gen.tupleToGen(caseEncoders[gen.TupleRep])
        }
        private val names = gen.names

        override def apply(a: A): Json =
          val casted       = gen.indexOfACasting(a)
          val idx          = casted.index
          val obj          = casted.value
          val typeName     = names.indexK(idx)
          val typeNameJson = Json.fromString(typeName)
          val encoder      = encoders.indexK(idx)

          val json = encoder(obj)
          json.asObject match
            case Some(_) => json.deepMerge(Json.obj("$type" -> typeNameJson))
            case None    => Json.obj("$type" -> typeNameJson, "$value" -> json)

    private inline def caseEncoders[XS <: Tuple]: Tuple.Map[XS, Encoder] = inline erasedValue[XS] match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: t) =>
        val headEncoder: Encoder[h] = summonFrom {
          case e: Encoder[`h`]             => e
          case given InlineHKDGeneric[`h`] => derived[h]
        }
        headEncoder *: caseEncoders[t]

    inline def derived[A](using gen: InlineHKDGeneric[A]): PerspectiveInlineEncoder[A] = inline gen match
      case gen: InlineHKDProductGeneric.Aux[A, gen.Gen] => derivedProductEncoder(using gen)
      case gen: InlineHKDSumGeneric.Aux[A, gen.Gen]     => derivedSumEncoder(using gen)

  case class Foo(i: Int, s: String, foobar: Long) derives PerspectiveInlineEncoder, PerspectiveInlineDecoder

  enum Bar derives PerspectiveInlineEncoder, PerspectiveInlineDecoder {
    case A(i: Int)
    case B(s: String)
    case C(foobar: Long)
  }
}
class InlineCodecTests extends AnyFunSuite {
  import InlineCodecTests.*

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
