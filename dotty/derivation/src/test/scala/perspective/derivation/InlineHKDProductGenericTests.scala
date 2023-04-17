package perspective.derivation

import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.util.NotGiven

import cats.Functor
import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import perspective.*

object InlineHKDProductGenericTests {
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
class InlineHKDProductGenericTests extends AnyFunSuite {
  import InlineHKDProductGenericTests.*

  // noinspection TypeAnnotation
  val instance = InlineHKDProductGeneric.derived[Foo]

  test("InlineHKDProductGeneric.from(to(_)) roundtrip is unchanged") {
    assert(instance.from(instance.to(Foo.value1)) === Foo.value1)
  }

  test("InlineHKDProductGeneric.names is correct") {
    assert(instance.toListK(instance.names) === Foo.names)
  }

  test("InlineHKDProductGeneric.typeName is correct") {
    assert((instance.typeName: String) === "Foo")
  }

  test("InlineHKDProductGeneric.nameToIndex is correct") {
    val value = instance.to(Foo.value1)

    val fromNamesValues = Foo.names.traverse((nameStr: String) =>
      instance
        .stringToName(nameStr)
        // Need the Any type here so that correct bytecode is generated
        .map[Any] { (name: instance.Names) =>
          val idx: instance.IndexAux[instance.FieldOf[name.type]] = instance.nameToIndex(name)
          val res: instance.FieldOf[name.type]                    = instance.indexK(value)(idx)

          res
        }
    )

    assert(fromNamesValues === Some(instance.toListK(instance.mapConst(value)[Any]([X] => (x: X) => x))))
  }

  test("InlineHKDProductGeneric.genFromTuple(tupleFromGen(_)) roundtrip is unchanged") {
    val v = instance.to(Foo.value1)
    assert(instance.tupleToGen[Id](instance.genToTuple(v)) === v)
  }

  test("InlineHKDProductGeneric.Gen[TC] is correct") {
    val instanceTcs = instance.genToTuple(instance.summonInstances[TC])
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

  // noinspection TypeAnnotation
  val nonInlineInstance = summon[HKDProductGeneric[Foo]]
  import nonInlineInstance.given

  case class W[A](a: A)

  def inlineTestFW[F1[_], F2[_], W[_]: Functor](
      name: String,
      startValue1: instance.Gen[F1] = instance.to(Foo.value1),
      startValue2: nonInlineInstance.Gen[F1] = nonInlineInstance.to(Foo.value1)
  )(f1: instance.Gen[F1] => W[instance.Gen[F2]], f2: nonInlineInstance.Gen[F1] => W[nonInlineInstance.Gen[F2]]): Unit =
    test(name) {
      val r1 = f1(startValue1).map(instance.genToTuple(_))
      val r2 = f2(startValue2).map(nonInlineInstance.genToTuple(_))
      assert(r1 === r2)
    }

  def inlineTestF[F1[_], F2[_]](
      name: String,
      startValue1: instance.Gen[F1] = instance.to(Foo.value1),
      startValue2: nonInlineInstance.Gen[F1] = nonInlineInstance.to(Foo.value1)
  )(f1: instance.Gen[F1] => instance.Gen[F2], f2: nonInlineInstance.Gen[F1] => nonInlineInstance.Gen[F2]): Unit =
    inlineTestFW[F1, F2, Id](name, startValue1, startValue2)(f1, f2)

  def inlineTestA[F1[_], A](
      name: String,
      startValue1: instance.Gen[F1] = instance.to(Foo.value1),
      startValue2: nonInlineInstance.Gen[F1] = nonInlineInstance.to(Foo.value1)
  )(f1: instance.Gen[F1] => A, f2: nonInlineInstance.Gen[F1] => A): Unit =
    test(name) {
      val r1 = f1(startValue1)
      val r2 = f2(startValue2)
      assert(r1 === r2)
    }

  inlineTestF[Id, W]("InlineHKDProductGeneric.mapK corresponds to HKDProductGeneric.mapK")(
    gen => instance.mapK(gen)([X] => (v: X) => W(v)),
    gen => {
      given FunctorKC[nonInlineInstance.Gen] = nonInlineInstance.representable
      gen.mapK([X] => (v: X) => W(v))
    }
  )

  inlineTestF[Id, Tuple2K[W, W]]("InlineHKDProductGeneric.map2K corresponds to HKDProductGeneric.map2K")(
    gen => instance.map2K[Id](gen)(gen)([X] => (v1: X, v2: X) => (W(v1), W(v2))),
    gen => gen.map2K(gen)([X] => (v1: X, v2: X) => (W(v1), W(v2)))
  )

  inlineTestF[Id, Const[Unit]]("InlineHKDProductGeneric.unitK corresponds to HKDProductGeneric.unitK")(
    gen => instance.unitK,
    gen => nonInlineInstance.representable.unitK
  )

  inlineTestF[Const[String], Const[String]](
    "InlineHKDProductGeneric.names corresponds to HKDProductGeneric.names",
    instance.mapK(instance.names)([X] => (name: instance.Names) => name: String), {
      given FunctorKC[nonInlineInstance.Gen] = nonInlineInstance.representable
      nonInlineInstance.names.mapK([X] => (name: nonInlineInstance.Names) => name: String)
    }
  )(
    gen => gen,
    gen => gen
  )

  inlineTestA[Const[String], String](
    "InlineHKDProductGeneric.foldLeftK corresponds to HKDProductGeneric.foldLeftK",
    instance.mapK(instance.names)([X] => (name: instance.Names) => name: String), {
      given FunctorKC[nonInlineInstance.Gen] = nonInlineInstance.representable
      nonInlineInstance.names.mapK([X] => (name: nonInlineInstance.Names) => name: String)
    }
  )(
    gen => instance.foldLeftK(gen)("")(acc => [Z] => (name: String) => acc + "|" + name),
    gen => gen.foldLeftK("")(acc => [Z] => (name: String) => acc + "|" + name)
  )

  // Traverse start

  inlineTestFW[Id, W, Id]("InlineHKDProductGeneric.traverseK[Id] corresponds to HKDProductGeneric.traverseK[Id]")(
    gen => instance.traverseK(gen)[Id, W]([X] => (x: X) => W(x)),
    gen => gen.traverseK[Id, W]([X] => (x: X) => W(x))
  )

  inlineTestFW[Id, W, Either[String, *]](
    "InlineHKDProductGeneric.traverseK[Either(Right)] corresponds to HKDProductGeneric.traverseK[Either(Right)]"
  )(
    gen => instance.traverseK(gen)[Either[String, *], W]([X] => (x: X) => Right(W(x))),
    gen => gen.traverseK[Either[String, *], W]([X] => (x: X) => Right(W(x)))
  )

  inlineTestFW[Id, W, Either[String, *]](
    "InlineHKDProductGeneric.traverseK[Either(Left)] corresponds to HKDProductGeneric.traverseK[Either(Left)]"
  )(
    gen => instance.traverseK(gen)[Either[String, *], W]([X] => (x: X) => Left("E")),
    gen => gen.traverseK[Either[String, *], W]([X] => (x: X) => Left("E"))
  )

  inlineTestFW[Id, W, Option](
    "InlineHKDProductGeneric.traverseK[Option(Some)] corresponds to HKDProductGeneric.traverseK[Option(Some)]"
  )(
    gen => instance.traverseK(gen)[Option, W]([X] => (x: X) => Some(W(x))),
    gen => gen.traverseK[Option, W]([X] => (x: X) => Some(W(x)))
  )

  inlineTestFW[Id, W, Option](
    "InlineHKDProductGeneric.traverseK[Option(None)] corresponds to HKDProductGeneric.traverseK[Option(None)]"
  )(
    gen => instance.traverseK(gen)[Option, W]([X] => (x: X) => None),
    gen => gen.traverseK[Option, W]([X] => (x: X) => None)
  )

  // Traverse end

  inlineTestF[Id, Compose2[Option, Id]](
    "InlineHKDProductGeneric.cosequenceK corresponds to HKDProductGeneric.cosequenceK"
  )(
    gen => instance.cosequenceK[Option, Id](Some(gen)),
    gen => nonInlineInstance.representable.cosequenceK(Some(gen))
  )

  inlineTestF[Id, Id]("InlineHKDProductGeneric.flatMapK corresponds to HKDProductGeneric.flatMapK")(
    gen => instance.flatMapK[Id](gen)[Id]([Z] => (v: Z) => gen),
    gen => gen.flatMapK[Id]([Z] => (v: Z) => gen)
  )

  inlineTestF[Id, Const[Int]]("InlineHKDProductGeneric.tabulateK corresponds to HKDProductGeneric.tabulateK")(
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

  inlineTestA[Id, Int](
    "InlineHKDProductGeneric.tabulateFoldLeft corresponds to HKDProductGeneric.indicesK.tabulateK"
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

  inlineTestFW[Id, W, Id](
    "InlineHKDProductGeneric.tabulateTraverseK[Id] corresponds to HKDProductGeneric.indicesK.traverseK[Id]"
  )(
    gen => instance.tabulateTraverseK[Id, W](idx => W(instance.indexK(gen)(idx))),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Id, W](
        [X] => (x: nonInlineInstance.Index[X]) => W(gen.indexK(x))
      )
  )

  inlineTestFW[Id, W, Either[String, *]](
    "InlineHKDProductGeneric.tabulateTraverseK[Either(Right)] corresponds to HKDProductGeneric.indicesK.traverseK[Either(Right)]"
  )(
    gen => instance.tabulateTraverseK[Either[String, *], W](idx => Right(W(instance.indexK(gen)(idx)))),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Either[String, *], W](
        [X] => (x: nonInlineInstance.Index[X]) => Right(W(gen.indexK(x)))
      )
  )

  inlineTestFW[Id, W, Either[String, *]](
    "InlineHKDProductGeneric.tabulateTraverseK[Either(Left)] corresponds to HKDProductGeneric.indicesK.traverseK[Either(Left)]"
  )(
    gen => instance.tabulateTraverseK[Either[String, *], W](idx => Left("E")),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Either[String, *], W](
        [X] => (x: nonInlineInstance.Index[X]) => Left("E")
      )
  )

  inlineTestFW[Id, W, Option](
    "InlineHKDProductGeneric.tabulateTraverseK[Option(Some)] corresponds to HKDProductGeneric.indicesK.traverseK[Option(Some)]"
  )(
    gen => instance.tabulateTraverseK[Option, W](idx => Some(W(instance.indexK(gen)(idx)))),
    gen =>
      nonInlineInstance.representable.indicesK.traverseK[Option, W](
        [X] => (x: nonInlineInstance.Index[X]) => Some(W(gen.indexK(x)))
      )
  )

  inlineTestFW[Id, W, Option](
    "InlineHKDProductGeneric.tabulateTraverseK[Option(None)] corresponds to HKDProductGeneric.indicesK.traverseK[Option(None)]"
  )(
    gen => instance.tabulateTraverseK[Option, W](idx => None),
    gen => nonInlineInstance.representable.indicesK.traverseK[Option, W]([X] => (x: nonInlineInstance.Index[X]) => None)
  )

  // TabulateTraverse end

  test(
    "InlineHKDProductGeneric.productElementId(a)(idx) corresponds to InlineHKDProductGeneric.indexK(InlineHKDProductGeneric.to(a))(idx)"
  ) {
    val a  = Foo.value1
    val to = instance.to(a)

    assert(instance.tabulateK[Id](i => instance.productElementId(a)(i)) === instance.tabulateK[Id](i => instance.indexK(to)(i)))
  }
}
