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
    assert(instance.from(instance.tupleToGen(instance.genToTuple(instance.to(Foo.value1)))) === Foo.value1)
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

  test(
    "HKDProductGeneric.productElementId(a)(idx) corresponds to HKDProductGeneric.indexK(HKDProductGeneric.to(a))(idx)"
  ) {
    val a  = Foo.value1
    val to = instance.to(a)
    val fromProductElement = instance.representable.tabulateK[Id, Nothing](
      [X] => (i: instance.Index[X]) => instance.productElementId(a)(i)
    )
    val fromIndexK = instance.representable.tabulateK[Id, Nothing]([X] => (i: instance.Index[X]) => to.indexK(i))

    assert(fromProductElement === fromIndexK)
  }

  test("HKDProductGeneric.tabulateFoldLeft corresponds to HKDProductGeneric.traverse.foldLeftK(instance.representable.indicesK)") {
    val names = instance.names

    val normal = instance.traverse.foldLeftK(instance.representable.indicesK)("")(b =>
      [X] => (i: instance.Index[X]) => b + names.indexK(i)
    )
    val quick = instance.tabulateFoldLeft("")(b => [X] => (i: instance.Index[X]) => b + names.indexK(i))

    assert(normal === quick)
  }

  case class W[A](a: A)

  test("HKDProductGeneric.tabulateTraverseK corresponds to HKDProductGeneric.traverse.traverseK(instance.representable.indicesK)") {
    val values = instance.to(Foo.value1)

    val normal = instance.traverse.traverseK(instance.representable.indicesK)(
      [X] => (i: instance.Index[X]) => Some(W(values.indexK(i))): Option[W[X]]
    )
    val quick = instance.tabulateTraverseK([X] => (i: instance.Index[X]) => Some(W(values.indexK(i))): Option[W[X]])

    assert(normal === quick)
  }

  test("HKDProductGeneric.tabulateTraverseKOption corresponds to HKDProductGeneric.traverse.traverseK(instance.representable.indicesK)") {
    val values = instance.to(Foo.value1)

    val normal = instance.traverse.traverseK(instance.representable.indicesK)(
      [X] => (i: instance.Index[X]) => Some(W(values.indexK(i))): Option[W[X]]
    )
    val quick = instance.tabulateTraverseKOption([X] => (i: instance.Index[X]) => Some(W(values.indexK(i))): Option[W[X]])

    assert(normal === quick)
  }

  test("HKDProductGeneric.tabulateTraverseKEither corresponds to HKDProductGeneric.traverse.traverseK(instance.representable.indicesK)") {
    val values = instance.to(Foo.value1)

    val normal = instance.traverse.traverseK(instance.representable.indicesK)(
      [X] => (i: instance.Index[X]) => Right(W(values.indexK(i))): Either[String, W[X]]
    )
    val quick = instance.tabulateTraverseKEither([X] => (i: instance.Index[X]) => Right(W(values.indexK(i))): Either[String, W[X]])

    assert(normal === quick)
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
