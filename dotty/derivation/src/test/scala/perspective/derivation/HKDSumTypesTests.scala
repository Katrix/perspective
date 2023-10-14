package perspective.derivation

import scala.util.NotGiven

object HKDSumTypesTests {
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
class HKDSumTypesTests {
  import HKDSumTypesTests.*

  // noinspection TypeAnnotation
  val instance = summon[HKDSumExtraTypes[Foo]]
  import instance.given

  //
  // Compile time tests
  //

  summon[instance.FieldOf["A"] =:= Foo.A]
  summon[instance.FieldOf["B"] =:= Foo.B]
  summon[instance.FieldOf["C"] =:= Foo.C]
  summon[instance.FieldOf["D"] =:= Foo.D]
  summon[instance.FieldOf["E"] =:= Foo.E]
  summon[instance.FieldOf["F"] =:= Foo.F]
  summon[instance.FieldOf["G"] =:= Foo.G]

  summon[instance.Names =:= ("A" | "B" | "C" | "D" | "E" | "F" | "G")]
  summon[instance.ElemTop =:= (Foo.A | Foo.B | Foo.C | Foo.D | Foo.E | Foo.F | Foo.G)]
  summon[instance.TupleRep =:= (Foo.A, Foo.B, Foo.C, Foo.D, Foo.E, Foo.F, Foo.G)]

  summon[NotGiven[instance.FieldOf[instance.Names] =:= Foo.A]]
}
