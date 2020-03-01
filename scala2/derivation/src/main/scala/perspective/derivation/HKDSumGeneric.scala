package perspective.derivation

import perspective.{Const, RepresentableKC, TraverseKC}

trait HKDSumGeneric[A] { self =>
  type Gen[_[_]]
  type Index[X]

  def typeName: String = ???

  def names: Gen[Const[String, *]]

  //Preferably we would say type Index[X <: A], but we can't
  def upcastIndexed[X](idx: Index[X], x: X): A
  def upcastIndex(idx: Index[_]): Index[_ <: A]

  def indexNameMap: Map[String, Index[_ <: A]]
  def indexOf[X <: A](x: X): Index[X]

  def to(a: A): Gen[Option]
  def from(a: Gen[Option]): Option[A]

  def representable: RepresentableKC.Aux[Gen, Index]
  def traverse: TraverseKC[Gen]

  object implicits extends SumImplicitsLowPriority[A, Gen] {
    implicit def representable: RepresentableKC.Aux[Gen, Index] = self.representable

    override private[derivation] def sumInstance: HKDSumGeneric.Aux[A, Gen] = self
  }
}
private[derivation] trait SumImplicitsLowPriority[A, Gen[_[_]]] {
  private[derivation] def sumInstance: HKDSumGeneric.Aux[A, Gen]

  implicit def traverse: TraverseKC[Gen] = sumInstance.traverse
}

object HKDSumGeneric {
  def apply[A](implicit gen: HKDSumGeneric[A]): HKDSumGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDSumGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }
}
