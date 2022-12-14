package perspective.derivation

import cats.{Applicative, Id}
import perspective._

trait HKDProductGeneric[A] {
  type Gen[_[_]]
  type Index[A]

  def typeName: String = ???

  def names: Gen[Const[String, *]]

  def to(a: A): Gen[Id]
  def from(gen: Gen[Id]): A

  def tabulateFoldLeft[B](start: B)(f: B => Index ~>: Const[B, *]): B

  def tabulateTraverseK[G[_], B[_]](f: Index ~>: Compose2[G, B, *])(implicit G: Applicative[G]): G[Gen[B]]

  def tabulateTraverseKOption[B[_]](f: Index ~>: Compose2[Option, B, *]): Option[Gen[B]]

  def tabulateTraverseKEither[E, B[_]](f: Index ~>: Compose2[Either[E, *], B, *]): Either[E, Gen[B]]

  def productElementId[X](a: A, index: Index[X]): X

  def representable: RepresentableKC.Aux[Gen, Index]
  def traverse: TraverseKC[Gen]

  val implicits: ProductImplicits[A, Gen, Index] = new ProductImplicits[A, Gen, Index](this)
}
private[derivation] class ProductImplicits[A, Gen[_[_]], Index0[_]](protected val gen: HKDProductGeneric.Aux[A, Gen] { type Index[A] = Index0[A] })
    extends ProductImplicitsLowPriority[A, Gen] {

  implicit def representable: RepresentableKC.Aux[Gen, Index0] = gen.representable
}
private[derivation] trait ProductImplicitsLowPriority[A, Gen[_[_]]] {

  protected def gen: HKDProductGeneric.Aux[A, Gen]

  implicit def traverse: TraverseKC[Gen] = gen.traverse
}

object HKDProductGeneric extends HKDProductGenericMacros {
  def apply[A](implicit gen: HKDProductGeneric[A]): HKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }
}
