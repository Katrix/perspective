package perspective.derivation

import scala.reflect.ClassTag

import io.circe._
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

object BarFast {
  case class Foo(bar: String, baz: Double, bin: Option[Char])
}

class JsonFastSpec extends AnyFunSuite with Matchers {
  import BarFast._

  implicit private val fooEncoder: Encoder[Foo] = EncoderDeriveFast.deriver[Foo].deriveProduct
  implicit private val fooDecoder: Decoder[Foo] = DecoderDeriveFast.deriver[Foo].deriveProduct

  private val foo     = Foo("bin", 5D, Some('d'))
  private val fooJson = Json.obj("bar" := "bin", "baz" := 5D, "bin" := 'd')

  def addType[A: ClassTag](json: Json): Json = {
    val className = implicitly[ClassTag[A]].runtimeClass.getName
    val scalaPath = className.replace('$', '.')
    json.mapObject(_.add("$type", scalaPath.asJson))
  }

  test("EncodeProductSimple") {
    fooEncoder(foo) should equal(fooJson)
  }

  test("DecodeProductSimple") {
    fooDecoder.decodeJson(fooJson).toOption should contain(foo)
  }
}
