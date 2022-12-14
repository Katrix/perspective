package perspective.derivation

import cats.Applicative
import perspective._

trait HKDSumGeneric[A] { self =>
  type Gen[_[_]]
  type Index[X]

  def typeName: String

  def names: Gen[Const[String, *]]

  // Preferably we would say type Index[X <: A], but we can't
  def upcastIndexed[X](idx: Index[X], x: X): A     = x.asInstanceOf[A]
  def upcastIndex[X](idx: Index[X]): Index[_ <: A] = idx.asInstanceOf[Index[_ <: A]]

  def nameToIndexMap: Map[String, Index[_ <: A]]
  def indexToNameMap: Map[Index[_ <: A], String]

  def indexOf[X <: A](x: X): Index[X]

  def to(a: A): Gen[Option] = {
    val index = indexOf(a)
    representable.tabulateK(new FunctionK[Index, Option] {
      override def apply[Z](i: Index[Z]): Option[Z] =
        if (i == index) Some(a.asInstanceOf[Z]) // This is safe as we know A = Z
        else None
    })
  }

  def from(a: Gen[Option]): Option[A] = {
    // This is safe. We can't use the widen method as it can't know about the contents of Gen, we do
    val aWiden = a.asInstanceOf[Gen[Const[Option[A], *]]]
    val asList = traverse.toListK[Option[A], Nothing](aWiden).flatten
    asList match {
      case Nil      => None    // No values present
      case a :: Nil => Some(a) // One value present
      case _        => None    // More than one value present
    }
  }

  def tabulateFoldLeft[B](start: B)(f: B => Index ~>: Const[B, *]): B

  def tabulateTraverseK[G[_], B[_]](f: Index ~>: Compose2[G, B, *])(implicit G: Applicative[G]): G[Gen[B]]

  def tabulateTraverseKOption[B[_]](f: Index ~>: Compose2[Option, B, *]): Option[Gen[B]]

  def tabulateTraverseKEither[E, B[_]](f: Index ~>: Compose2[Either[E, *], B, *]): Either[E, Gen[B]]

  def representable: RepresentableKC.Aux[Gen, Index]
  def traverse: TraverseKC[Gen]

  object implicits extends SumImplicitsLowPriority[A, Gen, Index] {
    implicit def representable: RepresentableKC.Aux[Gen, Index] = self.representable

    override private[derivation] def sumInstance: HKDSumGeneric.Aux[A, Gen] { type Index[A] = self.Index[A] } = self
  }
}
abstract private[derivation] class SumImplicitsLowPriority[A, Gen[_[_]], Index0[_]] {
  private[derivation] def sumInstance: HKDSumGeneric.Aux[A, Gen] { type Index[A] = Index0[A] }

  implicit def traverse: TraverseKC[Gen] = sumInstance.traverse
}

object HKDSumGeneric extends HKDSumGenericMacros {
  def apply[A](implicit gen: HKDSumGeneric[A]): HKDSumGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDSumGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }
}
