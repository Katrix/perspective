package perspective.derivation

import scala.compiletime.constValue

import cats.Functor
import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import perspective.*

object InlineHKDSumGenericTests {
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
class InlineHKDSumGenericTests extends AnyFunSuite {
  import InlineHKDSumGenericTests.*

  // noinspection TypeAnnotation
  val instance = InlineHKDSumGeneric.derived[Foo]

  summon[instance.TypeName =:= "Foo"]

  test("InlineHKDSumGeneric.from(to(_)) roundtrip is unchanged") {
    Foo.values.foreach { value =>
      assert(instance.from(instance.to(value)) === Some(value))
    }
  }

  test("InlineHKDSumGeneric.names is correct") {
    assert(instance.toListK(instance.names) === Foo.names)
  }

  test("InlineHKDSumGeneric.typeName is correct") {
    assert((instance.typeName: String) === "Foo")
  }

  test("InlineHKDSumGeneric.nameToIndex is correct") {
    Foo.values.foreach { fooValue =>
      val value = instance.to(fooValue)
      val fromNamesValues = Foo.names.traverse((nameStr: String) =>
        instance
          .stringToName(nameStr)
          // Need the Any type here so that correct bytecode is generated
          .map[Option[Any]] { (name: instance.Names) =>
            val idx: instance.Index = instance.nameToIndex(name)
            val res: Option[idx.X]  = value.indexK(idx)
            res
          }
      )

      assert(fromNamesValues === Some(instance.toListK(value.mapConst[Any]([X] => (x: Option[X]) => x))))
    }
  }

  test("InlineHKDSumGeneric.genFromTuple(tupleFromGen(_)) roundtrip is unchanged") {
    Foo.values.foreach { fooValue =>
      val v   = instance.to(fooValue)
      val tup = instance.genToTuple(v)
      val v2  = instance.tupleToGen[Option](tup)
      assert(v2 === v)
    }
  }

  test("InlineHKDSumGeneric.Gen[TC] is correct") {
    val instanceTcs = instance.genToTuple(instance.summonInstances[TC])
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

  // noinspection TypeAnnotation
  val nonInlineInstance = summon[HKDSumGeneric[Foo]]
  import nonInlineInstance.given

  case class W[A](a: A)

  def inlineTestFW[F1[_], F2[_], W[_]: Functor](
      name: String,
      startValue1: Foo => instance.Gen[F1] = instance.to(_),
      startValue2: Foo => nonInlineInstance.Gen[F1] = nonInlineInstance.to(_)
  )(f1: instance.Gen[F1] => W[instance.Gen[F2]], f2: nonInlineInstance.Gen[F1] => W[nonInlineInstance.Gen[F2]]): Unit =
    test(name) {
      Foo.values.foreach { fooValue =>
        val r1 = f1(startValue1(fooValue)).map(instance.genToTuple(_))
        val r2 = f2(startValue2(fooValue)).map(nonInlineInstance.genToTuple(_))
        assert(r1 === r2)
      }
    }

  def inlineTestF[F1[_], F2[_]](
      name: String,
      startValue1: Foo => instance.Gen[F1] = instance.to(_),
      startValue2: Foo => nonInlineInstance.Gen[F1] = nonInlineInstance.to(_)
  )(f1: instance.Gen[F1] => instance.Gen[F2], f2: nonInlineInstance.Gen[F1] => nonInlineInstance.Gen[F2]): Unit =
    inlineTestFW[F1, F2, Id](name, startValue1, startValue2)(f1, f2)

  def inlineTestA[F1[_], A](
      name: String,
      startValue1: Foo => instance.Gen[F1] = instance.to(_),
      startValue2: Foo => nonInlineInstance.Gen[F1] = nonInlineInstance.to(_)
  )(f1: instance.Gen[F1] => A, f2: nonInlineInstance.Gen[F1] => A): Unit =
    test(name) {
      Foo.values.foreach { fooValue =>
        val r1 = f1(startValue1(fooValue))
        val r2 = f2(startValue2(fooValue))
        assert(r1 === r2)
      }
    }

  inlineTestF[Option, Compose2[W, Option]]("InlineHKDSumGeneric.mapK corresponds to HKDSumGeneric.mapK")(
    gen => instance.mapK(gen)([X] => (v: Option[X]) => W(v)),
    gen => {
      given FunctorKC[nonInlineInstance.Gen] = nonInlineInstance.representable
      gen.mapK([X] => (v: Option[X]) => W(v))
    }
  )

  inlineTestF[Option, Tuple2K[Compose2[W, Option], Compose2[W, Option]]](
    "InlineHKDSumGeneric.map2K corresponds to HKDSumGeneric.map2K"
  )(
    gen => instance.map2K[Option](gen)(gen)([X] => (v1: Option[X], v2: Option[X]) => (W(v1), W(v2))),
    gen => gen.map2K(gen)([X] => (v1: Option[X], v2: Option[X]) => (W(v1), W(v2)))
  )

  inlineTestF[Option, Const[Unit]]("InlineHKDSumGeneric.unitK corresponds to HKDSumGeneric.unitK")(
    gen => instance.unitK,
    gen => nonInlineInstance.representable.unitK
  )

  inlineTestF[Const[String], Const[String]](
    "InlineHKDSumGeneric.names corresponds to HKDSumGeneric.names",
    _ => instance.mapK(instance.names)([X] => (name: instance.Names) => name: String),
    _ => {
      given FunctorKC[nonInlineInstance.Gen] = nonInlineInstance.representable
      nonInlineInstance.names.mapK([X] => (name: nonInlineInstance.Names) => name: String)
    }
  )(
    gen => gen,
    gen => gen
  )

  inlineTestA[Const[String], String](
    "InlineHKDSumGeneric.foldLeftK corresponds to HKDSumGeneric.foldLeftK",
    _ => instance.mapK(instance.names)([X] => (name: instance.Names) => name: String),
    _ => {
      given FunctorKC[nonInlineInstance.Gen] = nonInlineInstance.representable
      nonInlineInstance.names.mapK([X] => (name: nonInlineInstance.Names) => name: String)
    }
  )(
    gen => instance.foldLeftK(gen)("")(acc => [Z] => (name: String) => acc + "|" + name),
    gen => gen.foldLeftK("")(acc => [Z] => (name: String) => acc + "|" + name)
  )

  // Traverse start

  inlineTestFW[Option, Compose2[W, Option], Id](
    "InlineHKDSumGeneric.traverseK[Id] corresponds to HKDSumGeneric.traverseK[Id]"
  )(
    gen => instance.traverseK(gen)[Id, Compose2[W, Option]]([X] => (x: Option[X]) => W(x)),
    gen => gen.traverseK[Id, Compose2[W, Option]]([X] => (x: Option[X]) => W(x))
  )

  inlineTestFW[Option, Compose2[W, Option], Either[String, *]](
    "InlineHKDSumGeneric.traverseK[Either(Right)] corresponds to HKDSumGeneric.traverseK[Either(Right)]"
  )(
    gen => instance.traverseK(gen)[Either[String, *], Compose2[W, Option]]([X] => (x: Option[X]) => Right(W(x))),
    gen => gen.traverseK[Either[String, *], Compose2[W, Option]]([X] => (x: Option[X]) => Right(W(x)))
  )

  inlineTestFW[Option, Compose2[W, Option], Either[String, *]](
    "InlineHKDSumGeneric.traverseK[Either(Left)] corresponds to HKDSumGeneric.traverseK[Either(Left)]"
  )(
    gen => instance.traverseK(gen)[Either[String, *], Compose2[W, Option]]([X] => (x: Option[X]) => Left("E")),
    gen => gen.traverseK[Either[String, *], Compose2[W, Option]]([X] => (x: Option[X]) => Left("E"))
  )

  inlineTestFW[Option, Compose2[W, Option], Option](
    "InlineHKDSumGeneric.traverseK[Option(Some)] corresponds to HKDSumGeneric.traverseK[Option(Some)]"
  )(
    gen => instance.traverseK(gen)[Option, Compose2[W, Option]]([X] => (x: Option[X]) => Some(W(x))),
    gen => gen.traverseK[Option, Compose2[W, Option]]([X] => (x: Option[X]) => Some(W(x)))
  )

  inlineTestFW[Option, Compose2[W, Option], Option](
    "InlineHKDSumGeneric.traverseK[Option(None)] corresponds to HKDSumGeneric.traverseK[Option(None)]"
  )(
    gen => instance.traverseK(gen)[Option, Compose2[W, Option]]([X] => (x: Option[X]) => None),
    gen => gen.traverseK[Option, Compose2[W, Option]]([X] => (x: Option[X]) => None)
  )

  // Traverse end

  inlineTestF[Option, Compose2[Option, Option]](
    "InlineHKDSumGeneric.cosequenceK corresponds to HKDSumGeneric.cosequenceK"
  )(
    gen => instance.cosequenceK[Option, Option](Some(gen)),
    gen => nonInlineInstance.representable.cosequenceK(Some(gen))
  )

  inlineTestF[Option, Option]("InlineHKDSumGeneric.flatMapK corresponds to HKDSumGeneric.flatMapK")(
    gen => instance.flatMapK[Option](gen)[Option]([Z] => (v: Option[Z]) => gen),
    gen => gen.flatMapK[Option]([Z] => (v: Option[Z]) => gen)
  )

  inlineTestF[Option, Const[Int]]("InlineHKDSumGeneric.tabulateK corresponds to HKDSumGeneric.tabulateK")(
    gen => {
      var i: Int = 0
      instance.tabulateK { idx =>
        val tmp = i
        i += 1
        tmp
      }
    },
    gen => {
      var i: Int = 0
      nonInlineInstance.representable.tabulateK(
        [Z] =>
          (idx: nonInlineInstance.Index[Z]) => {
            val tmp = i
            i += 1
            tmp
        }
      )
    }
  )

  inlineTestA[Option, Int](
    "InlineHKDSumGeneric.tabulateFoldLeft corresponds to HKDSumGeneric.indicesK.tabulateK"
  )(
    gen => {
      instance.tabulateFoldLeft(0) { (acc, idx) =>
        acc + 1
      }
    },
    gen => {
      var i: Int = 0
      nonInlineInstance.representable.indicesK.foldLeftK(0)(acc =>
        [Z] =>
          (idx: nonInlineInstance.Index[Z]) => {
            acc + 1
        }
      )
    }
  )

  // TabulateTraverse start

  inlineTestFW[Option, Compose2[W, Option], Id](
    "InlineHKDSumGeneric.tabulateTraverseK[Id] corresponds to HKDSumGeneric.indicesK.traverseK[Id]"
  )(
    gen => instance.tabulateTraverseK[Id, Compose2[W, Option]](idx => W(instance.indexK(gen)(idx))),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Id, Compose2[W, Option]](
        [X] => (x: nonInlineInstance.Index[X]) => W(gen.indexK(x))
      )
  )

  inlineTestFW[Option, Compose2[W, Option], Either[String, *]](
    "InlineHKDSumGeneric.tabulateTraverseK[Either(Right)] corresponds to HKDSumGeneric.indicesK.traverseK[Either(Right)]"
  )(
    gen =>
      instance.tabulateTraverseK[Either[String, *], Compose2[W, Option]](idx => Right(W(instance.indexK(gen)(idx)))),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Either[String, *], Compose2[W, Option]](
        [X] => (x: nonInlineInstance.Index[X]) => Right(W(gen.indexK(x)))
      )
  )

  inlineTestFW[Option, Compose2[W, Option], Either[String, *]](
    "InlineHKDSumGeneric.tabulateTraverseK[Either(Left)] corresponds to HKDSumGeneric.indicesK.traverseK[Either(Left)]"
  )(
    gen => instance.tabulateTraverseK[Either[String, *], Compose2[W, Option]](idx => Left("E")),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Either[String, *], Compose2[W, Option]](
        [X] => (x: nonInlineInstance.Index[X]) => Left("E")
      )
  )

  inlineTestFW[Option, Compose2[W, Option], Option](
    "InlineHKDSumGeneric.tabulateTraverseK[Option(Some)] corresponds to HKDSumGeneric.indicesK.traverseK[Option(Some)]"
  )(
    gen => instance.tabulateTraverseK[Option, Compose2[W, Option]](idx => Some(W(instance.indexK(gen)(idx)))),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Option, Compose2[W, Option]](
        [X] => (x: nonInlineInstance.Index[X]) => Some(W(gen.indexK(x)))
      )
  )

  inlineTestFW[Option, Compose2[W, Option], Option](
    "InlineHKDSumGeneric.tabulateTraverseK[Option(None)] corresponds to HKDSumGeneric.indicesK.traverseK[Option(None)]"
  )(
    gen => instance.tabulateTraverseK[Option, Compose2[W, Option]](idx => None),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Option, Compose2[W, Option]](
        [X] => (x: nonInlineInstance.Index[X]) => None
      )
  )

  // TabulateTraverse end
}
