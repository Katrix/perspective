package perspective.derivation

import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.util.NotGiven

import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import perspective.*

object HKDProductGenericTests {
  case class Foo(a: Int, b: String, c: Double, d: Char, e: Boolean, f: Int, g: String)
  object Foo {
    val value1: Foo         = Foo(5, "foo", 3.14, ' ', false, -1, "bar")
    val names: List[String] = List("a", "b", "c", "d", "e", "f", "g")
  }

  trait TC[A]
  object TC {
    given TC[Int] with     {}
    given TC[String] with  {}
    given TC[Double] with  {}
    given TC[Char] with    {}
    given TC[Boolean] with {}
  }
}
class HKDProductGenericTests extends AnyFunSuite {
  import HKDProductGenericTests.*

  // noinspection TypeAnnotation
  val instance = summon[HKDProductGeneric[Foo]]
  import instance.given

  test("HKDProductGeneric.from(to(_)) roundtrip is unchanged") {
    assert(instance.from(instance.to(Foo.value1)) === Foo.value1)
  }

  test("HKDProductGeneric.names is correct") {
    assert(instance.names.toListK === Foo.names)
  }

  test("HKDProductGeneric.typeName is correct") {
    assert(instance.typeName === "Foo")
  }

  test("HKDProductGeneric.nameToIndex is correct") {
    given FunctorKC[instance.Gen] = instance.representable
    val value                     = instance.to(Foo.value1)

    val fromNamesValues = Foo.names.traverse((nameStr: String) =>
      instance
        .stringToName(nameStr)
        // Need the Any type here so that correct bytecode is generated
        .map[Any] { (name: instance.Names) =>
          val idx: instance.Index[instance.FieldOf[name.type]] = instance.nameToIndex(name)
          val res: instance.FieldOf[name.type]                 = value.indexK(idx)
          res
        }
    )

    assert(fromNamesValues === Some(value.mapConst([X] => (x: X) => x: Any).toListK))
  }

  test("HKDProductGeneric.genFromTuple(tupleFromGen(_)) roundtrip is unchanged") {
    val v = instance.to(Foo.value1)
    assert(instance.tupleToGen(instance.genToTuple(v)) === v)
  }

  test("HKDProductGeneric.Gen[TC] is correct") {
    val instanceTcs = instance.genToTuple(summon[instance.Gen[TC]])
    val tupleTcs = (
      summon[TC[Int]],
      summon[TC[String]],
      summon[TC[Double]],
      summon[TC[Char]],
      summon[TC[Boolean]],
      summon[TC[Int]],
      summon[TC[String]]
    )
    assert(instanceTcs === tupleTcs)
  }

  //
  // Compile time tests
  //

  summon[instance.TypeName =:= "Foo"]

  summon[instance.FieldOf["a"] =:= Int]
  summon[instance.FieldOf["b"] =:= String]
  summon[instance.FieldOf["c"] =:= Double]
  summon[instance.FieldOf["d"] =:= Char]
  summon[instance.FieldOf["e"] =:= Boolean]
  summon[instance.FieldOf["f"] =:= Int]
  summon[instance.FieldOf["g"] =:= String]

  summon[instance.Names =:= ("a" | "b" | "c" | "d" | "e" | "f" | "g")]
  summon[instance.ElemTop =:= (Int | String | Double | Char | Boolean | Int | String)]
  summon[instance.TupleRep =:= (Int, String, Double, Char, Boolean, Int, String)]

  summon[NotGiven[instance.FieldOf[instance.Names] =:= Int]]
}
