package perspective

import cats.kernel.BoundedEnumerable

trait BoundedRepresentableK[F[_[_], _]] extends RepresentableK[F]:
  case class ReprWrapper[X](repr: RepresentationK[X])

  val boundedRepresentableK: BoundedEnumerable[ReprWrapper[_]]

object BoundedRepresentableK:
  type Aux[F[_[_], _], RepresentationK0[_]] = BoundedRepresentableK[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }

/**
  * A version of [[BoundedRepresentableK]] without a normal type as well as a
  * higher kinded type.
  */
type BoundedRepresentableKC[F[_[_]]] = BoundedRepresentableK[IgnoreC[F]]
object BoundedRepresentableKC:
  type Aux[F[_[_]], RepresentationK0[_]] = BoundedRepresentableKC[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }
