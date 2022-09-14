package perspective.derivation

import scala.compiletime.constValue
import scala.deriving.Mirror

import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import perspective.*

object ProductKTests {
  type Tup = (Int, String, Double, Char, Boolean, Int, String)
  object Tup {
    val value1: Tup = (5, "foo", 3.14, ' ', false, -1, "bar")
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
class ProductKTests extends AnyFunSuite {
  import ProductKTests._

  // noinspection TypeAnnotation

  type InstanceTpe = RepresentableKC[ProductKPar[Tup]] & TraverseKC[ProductKPar[Tup]]

  given instance: InstanceTpe = ProductK.productKInstance[Tup]

  val tcInstancesK: ProductK[TC, Tup] = summon[ProductK[TC, Tup]]
  val tcInstancesList: List[TC[_]] =
    List(
      summon[TC[Int]],
      summon[TC[String]],
      summon[TC[Double]],
      summon[TC[Char]],
      summon[TC[Boolean]],
      summon[TC[Int]],
      summon[TC[String]]
    )

  test("ProductK.tabulateK indices correct") {
    assert(instance.tabulateK([X] => (rep: instance.RepresentationK[X]) => rep) === instance.indicesK)
  }

  test("ProductK.tabulateK corresponds to List.tabulate") {
    var i: Int = 0
    var j: Int = 0
    val prod   = instance.tabulateConst([X] => (_: instance.RepresentationK[X]) => { val t = i; i += 1; t }).toListK
    val list = List.tabulate(constValue[Tuple.Size[Tup]]) { _ =>
      val t = j; j += 1; t
    }

    assert(prod === list)
  }

  test("ProductK instances are correct") {
    assert(tcInstancesK.mapConst([X] => (tc: TC[X]) => tc).toListK === tcInstancesList)
  }

  test("ProductK.foldLeft corresponds to List.foldLeft") {
    assert(
      tcInstancesK.foldLeftK(List(1, 2, 3): List[Any])(acc => [X] => (tc: TC[X]) => tc :: acc) ===
        tcInstancesList.foldLeft(List(1, 2, 3): List[Any])((acc, tc) => tc :: acc)
    )
  }

  test("ProductK.traverseK corresponds to List.traverse") {
    assert(
      tcInstancesK.traverseConst([X] => (tc: TC[X]) => Some(tc): Option[TC[_]]).map(_.toListK) ===
        tcInstancesList.traverse(tc => Some(tc): Option[TC[_]])
    )
  }
}
