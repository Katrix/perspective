package perspective.derivation

import scala.reflect.ClassTag

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import perspective.syntax.all._

class GenericSpec extends AnyFunSuite with Matchers {

  def scalaPath[A: ClassTag]: String = {
    val className = implicitly[ClassTag[A]].runtimeClass.getName
    className.replace('$', '.')
  }

  private val foo = HKDProductGeneric[Bar.Foo]
  private val baz = HKDProductGeneric[Bar.Baz]
  private val bar = HKDSumGeneric[Bar]

  import bar.implicits._
  import baz.implicits._
  import foo.implicits._

  test("typeName") {
    foo.typeName should equal(scalaPath[Bar.Foo])
    baz.typeName should equal(scalaPath[Bar.Baz])
    bar.typeName should equal(scalaPath[Bar])
  }

  test("names") {
    foo.names.toListKC should equal(List("bar", "baz", "bin"))
    baz.names.toListKC should equal(List("uuid", "name"))
    bar.names.toListKC.toSet should equal(Set(scalaPath[Bar.Foo], scalaPath[Bar.Baz], scalaPath[Bar.Quox]))
  }

  test("nameToIndexMap") {
    bar.nameToIndexMap.keys.toSet should equal(Set(scalaPath[Bar.Foo], scalaPath[Bar.Baz], scalaPath[Bar.Quox]))
  }
}
