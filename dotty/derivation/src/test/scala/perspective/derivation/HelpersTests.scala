package perspective.derivation

import scala.compiletime.*

import Helpers.*
import org.scalatest.funsuite.AnyFunSuite

class HelpersTests extends AnyFunSuite {

  test("constValueTupleOptimized correct") {
    inline def testTuple[A <: Tuple] = assert(constValueTupleOptimized[A] === constValueTuple[A])

    testTuple[EmptyTuple]
    testTuple[Tuple1[1]]
    testTuple[(1, 2)]
    testTuple[0 *: (1, 2)]
    testTuple[-1 *: 0 *: (1, 2)]
    testTuple[0 *: 1 *: 2 *: EmptyTuple]
    testTuple[(1, 2, 3)]
    testTuple[0 *: (1, 2, 3)]
    testTuple[-1 *: 0 *: (1, 2, 3)]
    testTuple[0 *: 2 *: 3 *: EmptyTuple]
  }

  test("summonAllOptimized correct") {
    inline def testTuple[A <: Tuple] =
      val t1 = summonAllOptimized[Tuple.Map[A, ValueOf]]
      val t2 = summonAll[Tuple.Map[A, ValueOf]]

      assert(
        t1.toList.map(_.asInstanceOf[ValueOf[Any]].value) === t2.toList.map(_.asInstanceOf[ValueOf[Any]].value)
      )

    testTuple[EmptyTuple]
    testTuple[Tuple1[1]]
    testTuple[(1, 2)]
    testTuple[0 *: (1, 2)]
    testTuple[-1 *: 0 *: (1, 2)]
    testTuple[0 *: 1 *: 2 *: EmptyTuple]
    testTuple[(1, 2, 3)]
    testTuple[0 *: (1, 2, 3)]
    testTuple[-1 *: 0 *: (1, 2, 3)]
    testTuple[0 *: 2 *: 3 *: EmptyTuple]
  }
}
