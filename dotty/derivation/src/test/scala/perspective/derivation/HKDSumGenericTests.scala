package perspective.derivation

import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.util.NotGiven

import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import perspective.*

object HKDSumGenericTests {
  sealed trait Foo
  object Foo {
    case class A(a: Int)     extends Foo
    case class B(b: String)  extends Foo
    case class C(c: Double)  extends Foo
    case class D(d: Char)    extends Foo
    case class E(e: Boolean) extends Foo
    case class F(f: Int)     extends Foo
    case class G(g: String)  extends Foo

    val values: List[Foo] = List(
      Foo.A(5),
      Foo.B("foo"),
      Foo.C(3.14),
      Foo.D(' '),
      Foo.E(false),
      Foo.F(-1),
      Foo.G("bar")
    )
    val names: List[String] = List("A", "B", "C", "D", "E", "F", "G")
  }

  trait TC[A]
  object TC {
    given TC[Foo.A] with {}
    given TC[Foo.B] with {}
    given TC[Foo.C] with {}
    given TC[Foo.D] with {}
    given TC[Foo.E] with {}
    given TC[Foo.F] with {}
    given TC[Foo.G] with {}
  }
}
class HKDSumGenericTests extends AnyFunSuite {
  import HKDSumGenericTests.*

  // noinspection TypeAnnotation
  val instance = summon[HKDSumGeneric[Foo]]
  import instance.given

  summon[instance.TypeName =:= "Foo"]

  test("HKDSumGeneric.from(to(_)) roundtrip is unchanged") {
    Foo.values.foreach { value =>
      val to   = instance.to(value)
      val from = instance.from(to)
      assert(instance.from(instance.to(value)) === Some(value))
    }
  }

  test("HKDSumGeneric.names is correct") {
    assert(instance.names.toListK === Foo.names)
  }

  test("HKDSumGeneric.typeName is correct") {
    assert(instance.typeName === "Foo")
  }

  test("HKDSumGeneric.nameToIndex is correct") {
    given FunctorKC[instance.Gen] = instance.representable
    Foo.values.foreach { fooValue =>
      val value = instance.to(fooValue)
      val fromNamesValues = Foo.names.traverse((nameStr: String) =>
        instance
          .stringToName(nameStr)
          // Need the Any type here so that correct bytecode is generated
          .map[Option[Any]] { (name: instance.Names) =>
            val idx: instance.IdxWrapper[_] = instance.nameToIndex(name)
            val res: Option[_]              = value.indexK(idx.idx)
            res
          }
      )

      assert(fromNamesValues === Some(value.mapConst([X] => (x: Option[X]) => x: Option[Any]).toListK))
    }
  }

  test("HKDSumGeneric.genFromTuple(tupleFromGen(_)) roundtrip is unchanged") {
    Foo.values.foreach { fooValue =>
      assert(instance.from(instance.tupleToGen(instance.genToTuple(instance.to(fooValue)))) === Some(fooValue))
    }
  }

  test("HKDSumGeneric.Gen[TC] is correct") {
    val instanceTcs = instance.genToTuple(summon[instance.Gen[TC]])

    val tupleTcs = (
      summon[TC[Foo.A]],
      summon[TC[Foo.B]],
      summon[TC[Foo.C]],
      summon[TC[Foo.D]],
      summon[TC[Foo.E]],
      summon[TC[Foo.F]],
      summon[TC[Foo.G]]
    )
    assert(instanceTcs === tupleTcs)
  }

  test(
    "HKDSumGeneric.tabulateFoldLeft corresponds to HKDSumGeneric.traverse.foldLeftK(instance.representable.indicesK)"
  ) {
    val names = instance.names

    val normal = instance.traverse.foldLeftK(instance.representable.indicesK)("")(b =>
      [X] => (i: instance.Index[X]) => b + names.indexK(i)
    )
    val quick = instance.tabulateFoldLeft("")(b => [X] => (i: instance.Index[X]) => b + names.indexK(i))

    assert(normal === quick)
  }

  test(
    "HKDSumGeneric.tabulateTraverseK corresponds to HKDSumGeneric.traverse.traverseK(instance.representable.indicesK)"
  ) {
    Foo.values.foreach { value =>
      val values = instance.to(value)

      val normal = instance.traverse.traverseK(instance.representable.indicesK)(
        [X] => (i: instance.Index[X]) => Some(values.indexK(i)): Option[Option[X]]
      )
      val quick = instance.tabulateTraverseK([X] => (i: instance.Index[X]) => Some(values.indexK(i)): Option[Option[X]])

      assert(normal === quick)
    }
  }

  test(
    "HKDSumGeneric.tabulateTraverseKOption corresponds to HKDSumGeneric.traverse.traverseK(instance.representable.indicesK)"
  ) {
    Foo.values.foreach { value =>
      val values = instance.to(value)

      val normal = instance.traverse.traverseK(instance.representable.indicesK)(
        [X] => (i: instance.Index[X]) => Some(values.indexK(i)): Option[Option[X]]
      )
      val quick =
        instance.tabulateTraverseKOption([X] => (i: instance.Index[X]) => Some(values.indexK(i)): Option[Option[X]])

      assert(normal === quick)
    }
  }

  test(
    "HKDSumGeneric.tabulateTraverseKEither corresponds to HKDSumGeneric.traverse.traverseK(instance.representable.indicesK)"
  ) {
    Foo.values.foreach { value =>
      val values = instance.to(value)

      val normal = instance.traverse.traverseK(instance.representable.indicesK)(
        [X] => (i: instance.Index[X]) => Right(values.indexK(i)): Either[String, Option[X]]
      )
      val quick = instance.tabulateTraverseKEither(
        [X] => (i: instance.Index[X]) => Right(values.indexK(i)): Either[String, Option[X]]
      )

      assert(normal === quick)
    }
  }
}
