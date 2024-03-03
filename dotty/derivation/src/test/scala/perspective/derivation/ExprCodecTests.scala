package perspective.derivation

import io.circe.*
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite

object ExprCodecTests {
  import ExprCodecTestsMacros.*
  case class Foo(i: Int, s: String, foobar: Long) derives PerspectiveExprEncoder, PerspectiveExprDecoder
}
class ExprCodecTests extends AnyFunSuite {

  import ExprCodecTests.*

  test("Foo as json") {
    assert(Foo(5, "bar", 9L).asJson === Json.obj("i" -> 5.asJson, "s" -> "bar".asJson, "foobar" -> 9L.asJson))
  }

  test("Foo from json") {
    assert(Json.obj("i" -> 5.asJson, "s" -> "bar".asJson, "foobar" -> 9L.asJson).as[Foo].contains(Foo(5, "bar", 9L)))
  }
}
