package perspective.derivation

import cats.Id
import perspective._

trait HKDProductGeneric[A] {
  type Gen[_[_]]

  def typeName: String = ???

  def names: Gen[Const[String, *]]

  def to(a: A): Gen[Id]
  def from(gen: Gen[Id]): A

  def representable: RepresentableKC[Gen]
  def traverse: TraverseKC[Gen]

  val implicits: ProductImplicits[A, Gen] = new ProductImplicits[A, Gen](this)
}
private[derivation] class ProductImplicits[A, Gen[_[_]]](gen: HKDProductGeneric.Aux[A, Gen])
    extends ProductImplicitsLowPriority(gen) {

  implicit def representable: RepresentableKC[Gen] = gen.representable
}
abstract private[derivation] class ProductImplicitsLowPriority[A, Gen[_[_]]](gen: HKDProductGeneric.Aux[A, Gen]) {

  implicit def traverse: TraverseKC[Gen] = gen.traverse
}

object HKDProductGeneric extends HKDProductGenericMacros {
  def apply[A](implicit gen: HKDProductGeneric[A]): HKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }
}
