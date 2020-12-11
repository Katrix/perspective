package perspective.examples

import cats.Id
import cats.instances.either._
import perspective._
import perspective.derivation._
import perspective.syntax.all._

case class Foo(i: Int, s: String)
case class Bar(
    i1_1: Int,
    i1_2: Int,
    i1_3: Int,
    i1_4: Int,
    i1_5: Int,
    i1_6: Int,
    i1_7: Int,
    i1_8: Int,
    i1_9: Int,
    i1_10: Int,
    i1_11: Int,
    i1_12: Int,
    i1_13: Int,
    i1_14: Int,
    i1_15: Int,
    i1_16: Int,
    i1_17: Int,
    i1_18: Int,
    i1_19: Int,
    i1_20: Int,
    i1_21: Int,
    i1_22: Int
)
object Foo {
  //implicitly[Decoder[Foo]]
  //implicitly[Encoder[Foo]]

  //implicitly[Encoder[(String, Int)]]

  //implicitly[Decoder[Bar]]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
}

sealed trait SumTest
object SumTest {
  case class Foo(a: String, b: Int)    extends SumTest
  case class Bar(c: Double, b: String) extends SumTest

  HKDSumGeneric.materializeHKDSum[SumTest]
}

sealed trait Sum23Test
object Sum23Test {
  case class S1(a: String)  extends Sum23Test
  case class S2(a: String)  extends Sum23Test
  case class S3(a: String)  extends Sum23Test
  case class S4(a: String)  extends Sum23Test
  case class S5(a: String)  extends Sum23Test
  case class S6(a: String)  extends Sum23Test
  case class S7(a: String)  extends Sum23Test
  case class S8(a: String)  extends Sum23Test
  case class S9(a: String)  extends Sum23Test
  case class S10(a: String) extends Sum23Test
  case class S11(a: String) extends Sum23Test
  case class S12(a: String) extends Sum23Test
  case class S13(a: String) extends Sum23Test
  case class S14(a: String) extends Sum23Test
  case class S15(a: String) extends Sum23Test
  case class S16(a: String) extends Sum23Test
  case class S17(a: String) extends Sum23Test
  case class S18(a: String) extends Sum23Test
  case class S19(a: String) extends Sum23Test
  case class S20(a: String) extends Sum23Test
  case class S21(a: String) extends Sum23Test
  case class S22(a: String) extends Sum23Test
  case class S23(a: String) extends Sum23Test

  HKDSumGeneric.materializeHKDSum[Sum23Test]
}
