package perspective.examples

import perspective.macros.hkd

@hkd case class HKDAnnotationsTest[F[_]](
    a: F[Int],
    rest: HKDAnnotationsTest2[F],
    b: F[String]
)

@hkd case class HKDAnnotationsTest2[F[_]](
    c: F[Double],
    d: F[Char]
) {
  println("Foo5")
}

/*
object MyObj {
  @hkd case class HKDAnnotationsTest3[F[_]](
      c: F[Double],
      d: F[Char]
  ) {
    println("Foo3")
  }
}
 */
