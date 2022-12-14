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

object InlineUnrollingCodecTests {

  trait PerspectiveInlineEncoder[A] extends Encoder[A]
  object PerspectiveInlineEncoder:

    inline def derivedProductEncoder[A](using gen: InlineHKDProductGeneric[A]): PerspectiveInlineEncoder[A] =
      new PerspectiveInlineEncoder[A]:
        private val names    = gen.names
        private val encoders = gen.summonInstances[Encoder]

        override def apply(a: A): Json =
          val list = gen.tabulateFoldLeft(Nil: List[(String, Json)], unrolling = true)((acc, idx) =>
            val js = gen.lateInlineMatch {
              a.productElementIdExact(idx) match {
                case p: Byte    => Json.fromInt(p)
                case p: Char    => Json.fromString(p.toString)
                case p: Short   => Json.fromInt(p)
                case p: Int     => Json.fromInt(p)
                case p: Long    => Json.fromLong(p)
                case p: Float   => Json.fromFloatOrString(p)
                case p: Double  => Json.fromDoubleOrString(p)
                case p: Boolean => Json.fromBoolean(p)
                case p: String  => Json.fromString(p)
                case other      => encoders.indexK(idx)(other)
              }
            }

            (names.indexK(idx), js) :: acc
          )

          Json.obj(list: _*)

    inline def derived[A](using gen: InlineHKDGeneric[A]): PerspectiveInlineEncoder[A] = inline gen match
      case gen: InlineHKDProductGeneric.Aux[A, gen.Gen] => derivedProductEncoder(using gen)

  case class Foo(i: Int, s: String, foobar: Long) derives PerspectiveInlineEncoder
}
class InlineUnrollingCodecTests extends AnyFunSuite {
  import InlineUnrollingCodecTests.*

  test("Foo as json") {
    assert(Foo(5, "bar", 9L).asJson === Json.obj("i" -> 5.asJson, "s" -> "bar".asJson, "foobar" -> 9L.asJson))
  }
}
