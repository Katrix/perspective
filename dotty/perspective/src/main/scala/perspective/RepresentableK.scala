package perspective

import scala.language.implicitConversions

import cats.Functor
import cats.syntax.all._

/** A higher kinded [[cats.Representable]]. */
trait RepresentableK[F[_[_], _]] extends MonadK[F] with DistributiveK[F]:

  type RepresentationK[_]

  /** A higher kinded equivalent of [[cats.Representable.tabulate]]. */
  def tabulateK[A[_], C](f: RepresentationK ~>: A): F[A, C]

  /** Helper function that calls [[tabulateK]] with [[Const]]. */
  def tabulateConst[A, C](f: RepresentationK ~>#: A): F[Const[A], C] =
    tabulateK(f)

  /** Access the indices or the representation of this type. */
  def indicesK[C]: F[RepresentationK, C] = tabulateK(FunctionK.identity)

  extension [A[_]](a: ValueK[A])
    override def pure[C]: F[A, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => a[Z]())

  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Representable.index]]. */
    def indexK: RepresentationK ~>: A

    override def mapK[B[_]](f: A ~>: B): F[B, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r)))

    override def map2K[B[_], Z[_]](fb: F[B, C])(f: [X] => (A[X], B[X]) => Z[X]): F[Z, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r), fb.indexK(r)))

    override def flatMapK[B[_]](f: A ~>: F[B, *]): F[B, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r)).indexK(r))

  extension [G[_]: Functor, A[_], C](gfa: G[F[A, C]])
    override def cosequenceK: F[Compose2[G, A], C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => gfa.map(fa => fa.indexK(r)))

object RepresentableK:
  type Aux[F[_[_], _], RepresentationK0[_]] = RepresentableK[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }

/**
  * A version of [[RepresentableK]] without a normal type as well as a higher
  * kinded type.
  */
type RepresentableKC[F[_[_]]] = RepresentableK[IgnoreC[F]]
object RepresentableKC:
  type Aux[F[_[_]], RepresentationK0[_]] = RepresentableKC[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }
