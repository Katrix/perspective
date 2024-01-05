package perspective

import scala.language.implicitConversions

import cats.syntax.all.*
import cats.{Functor, Representable}

/** A higher kinded [[cats.Representable]]. */
trait RepresentableK[F[_[_], _]] extends MonadK[F] with DistributiveK[F]:

  type RepresentationK[_]

  /** A higher kinded equivalent of [[cats.Representable.tabulate]]. */
  def tabulateK[A[_], C](f: RepresentationK :~>: A): F[A, C]

  /** Helper function that calls [[tabulateK]] with [[Const]]. */
  def tabulateConst[A, C](f: RepresentationK :~>#: A): F[Const[A], C] =
    tabulateK(f)

  /** Access the indices or the representation of this type. */
  def indicesK[C]: F[RepresentationK, C] = tabulateK(FunctionK.identity)

  extension [A[_]](a: ValueK[A])
    override def pure[C]: F[A, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => a[Z]())

  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Representable.index]]. */
    def indexK[Z](i: RepresentationK[Z]): A[Z]

    override def mapK[B[_]](f: A :~>: B): F[B, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r)))

    override def map2K[B[_], Z[_]](fb: F[B, C])(f: [X] => (A[X], B[X]) => Z[X]): F[Z, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r), fb.indexK(r)))

    override def flatMapK[B[_]](f: A :~>: F[B, *]): F[B, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r)).indexK(r))

  extension [G[_]: Functor, A[_], C](gfa: G[F[A, C]])
    override def cosequenceK: F[Compose2[G, A], C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => gfa.map(fa => fa.indexK(r)))

object RepresentableK:
  type Aux[F[_[_], _], RepresentationK0[_]] = RepresentableK[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }

  given idInstanceC[A]: RepresentableKC[IdFC[A]] = instances.idInstanceC[A]

/**
  * A version of [[RepresentableK]] without a normal type as well as a higher
  * kinded type.
  */
type RepresentableKC[F[_[_]]] = RepresentableK[IgnoreC[F]]
object RepresentableKC:
  type Aux[F[_[_]], RepresentationK0[_]] = RepresentableKC[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }

  given idInstanceC[A]: RepresentableKC.Aux[IdFC[A], [Z] =>> Finite[1]] = perspective.instances.idInstanceC[A]

  given composeCats[F[_], G[_[_]], R1, R2[_]](
      using F: Representable.Aux[F, R1],
      G: RepresentableKC.Aux[G, R2]
  ): RepresentableKC.Aux[[H[_]] =>> F[G[H]], [X] =>> (R1, R2[X])] = new RepresentableKC[[H[_]] =>> F[G[H]]] {
    override type RepresentationK[A] = (R1, R2[A])

    extension [A[_], C](fa: F[G[A]])
      override def indexK[Z](i: (R1, R2[Z])): A[Z] =
        F.index(fa)(i._1).indexK(i._2)

    override def tabulateK[A[_], C](f: RepresentationK :~>: A): F[G[A]] =
      F.tabulate(r1 => G.tabulateK([X] => (r2: R2[X]) => f((r1, r2))))
  }

  given composeId[F[_], R1, X](using F: Representable.Aux[F, R1]): RepresentableKC[[H[_]] =>> F[H[X]]] = composeCats[F, IdFC[X], R1, [Z] =>> Finite[1]]
