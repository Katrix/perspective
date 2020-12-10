package perspective.derivation

case class HKDProductTest(
    a: String,
    b: Int
) derives HKDProductGeneric

enum Foo derives HKDGeneric {
  case A(a: String)
  case B(b: Int)
}

//val i = HKDProductGeneric.derived[HKDTest]
//val j = HKDProductGeneric.derived[HKDTest]
