package perspective.derivation

import cats.Id

import shapeless.{Const => _, _}
import perspective._

trait HKDProductGeneric[A] { self =>
  type Gen[_[_]]

  def typeName: String = ???

  def names: Gen[Const[String, *]]

  def to(a: A): Gen[Id]
  def from(gen: Gen[Id]): A

  def representable: RepresentableKC[Gen]
  def traverse: TraverseKC[Gen]

  object implicits extends ProductImplicitsLowPriority[A, Gen] {
    implicit def representable: RepresentableKC[Gen] = self.representable

    override private[derivation] def productInstance: HKDProductGeneric.Aux[A, Gen] = self
  }
}
private[derivation] trait ProductImplicitsLowPriority[A, Gen[_[_]]] {
  private[derivation] def productInstance: HKDProductGeneric.Aux[A, Gen]

  implicit def traverse: TraverseKC[Gen] = productInstance.traverse
}

object HKDProductGeneric extends HKDProductGenericInstances {
  def apply[A](implicit gen: HKDProductGeneric[A]): HKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }
}
