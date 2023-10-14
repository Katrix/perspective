package perspective.derivation

import scala.util.NotGiven

object HKDProductTypesTest {
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
class HKDProductTypesTest {
  import HKDProductTypesTest.*

  // noinspection TypeAnnotation
  val instance = HKDProductExtraTypes.derived[Foo]

  //
  // Compile time tests
  //

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
