package perspective.derivation

import io.circe._
import io.circe.syntax._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.reflect.ClassTag

sealed trait Bar
object Bar {
  case class Foo(bar: String, baz: Double, bin: Option[Char]) extends Bar
  case class Baz(uuid: UUID, name: String)                    extends Bar
  case class Quox(single: Double)                             extends Bar
}

class JsonSpec extends AnyFunSuite with Matchers {
  import Bar._

  implicit private val fooEncoder: Encoder[Foo] = EncoderDerive.deriver[Foo].deriveProduct
  implicit private val fooDecoder: Decoder[Foo] = DecoderDerive.deriver[Foo].deriveProduct

  implicit private val bazEncoder: Encoder[Baz] = EncoderDerive.deriver[Baz].deriveProduct
  implicit private val bazDecoder: Decoder[Baz] = DecoderDerive.deriver[Baz].deriveProduct

  implicit private val quoxEncoder: Encoder[Quox] = Encoder.encodeDouble.contramap(_.single)
  implicit private val quoxDecoder: Decoder[Quox] = Decoder.decodeDouble.map(Quox)

  implicit private val barEncoder: Encoder[Bar] = EncoderDerive.deriver[Bar].deriveSum
  implicit private val barDecoder: Decoder[Bar] = DecoderDerive.deriver[Bar].deriveSum

  private val foo     = Foo("bin", 5D, Some('d'))
  private val fooJson = Json.obj("bar" := "bin", "baz" := 5D, "bin" := 'd')

  private val baz     = Baz(UUID.fromString("8271c14c-9a03-4d7f-8e3d-9c80046e3f61"), "foobar")
  private val bazJson = Json.obj("uuid" := UUID.fromString("8271c14c-9a03-4d7f-8e3d-9c80046e3f61"), "name" := "foobar")

  private val quox     = Quox(42.123D)
  private val quoxJson = Json.obj("$value" := 42.123D)

  def addType[A: ClassTag](json: Json): Json = {
    val className = implicitly[ClassTag[A]].runtimeClass.getName
    val scalaPath = className.replace('$', '.')
    json.mapObject(_.add("$type", scalaPath.asJson))
  }

  test("EncodeProductSimple") {
    fooEncoder(foo) should equal(fooJson)
    bazEncoder(baz) should equal(bazJson)
  }

  test("DecodeProductSimple") {
    fooDecoder.decodeJson(fooJson).toOption should contain(foo)
    bazDecoder.decodeJson(bazJson).toOption should contain(baz)
  }

  test("EncodeSum") {
    barEncoder(foo) should equal(addType[Foo](fooJson))
    barEncoder(baz) should equal(addType[Baz](bazJson))
    barEncoder(quox) should equal(addType[Quox](quoxJson))
  }

  test("DecodeSum") {
    barDecoder.decodeJson(addType[Foo](fooJson)).toOption should contain(foo)
    barDecoder.decodeJson(addType[Baz](bazJson)).toOption should contain(baz)
    barDecoder.decodeJson(addType[Quox](quoxJson)).toOption should contain(quox)
  }
}
