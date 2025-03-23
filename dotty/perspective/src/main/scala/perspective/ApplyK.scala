package perspective

import cats.Apply
import cats.syntax.all.*

/** A higher kinded [[cats.Apply]] typeclass. */
trait ApplyK[F[_[_], _]] extends FunctorK[F]:
  /** A higher kinded equivalent of [[cats.Apply.ap]]. */
  extension [A[_], B[_], C](ff: F[[D] =>> A[D] => B[D], C])
    def ap(fa: F[A, C]): F[B, C] =
      ff.map2K(fa)([Z] => (f: A[Z] => B[Z], a: A[Z]) => f(a))

  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Apply.map2]]. */
    def map2K[B[_], Z[_]](fb: F[B, C])(f: [X] => (A[X], B[X]) => Z[X]): F[Z, C]

    /** Helper function that calls [[map2K]] with [[Const]]. */
    inline def map2Const[B[_], Z](fb: F[B, C])(f: [X] => (A[X], B[X]) => Z): F[Const[Z], C] =
      fa.map2K(fb)(f)

    /** A higher kinded equivalent of [[cats.Apply.product]]. */
    def tupledK[B[_]](fb: F[B, C]): F[Tuple2K[A, B], C] =
      fa.map2K(fb)([Z] => (a: A[Z], b: B[Z]) => (a, b))

object ApplyK:
  given idInstanceC[A]: ApplyKC[IdFC[A]] = instances.idInstanceC[A]

  given composeCatsOutside[F[_], G[_[_]]](using F: Apply[F], G: ApplyKC[G]): ApplyKC[[H[_]] =>> F[G[H]]] with
    extension [A[_], C](fa: F[G[A]])
      override def map2K[B[_], Z[_]](fb: F[G[B]])(f: [Y] => (A[Y], B[Y]) => Z[Y]): F[G[Z]] =
        fa.map2(fb)((a, b) => a.map2K(b)(f))

      override def mapK[B[_]](f: A :~>: B): F[G[B]] = fa.map(_.mapK(f))

  given composeId[F[_], X](using F: Apply[F]): ApplyKC[[H[_]] =>> F[H[X]]] = composeCatsOutside[F, IdFC[X]]

  given composeCatsInside[F[_[_]], G[_]](using F: ApplyKC[F], G: Apply[G]): ApplyKC[[H[_]] =>> F[Compose2[G, H]]] with {
    extension [A[_], C](fga: F[Compose2[G, A]])
      override def mapK[B[_]](f: A :~>: B): F[Compose2[G, B]] = F.mapK(fga)([X] => (ga: G[A[X]]) => ga.map(a => f(a)))

      override def map2K[B[_], Z[_]](fgb: F[Compose2[G, B]])(f: [X] => (A[X], B[X]) => Z[X]): F[Compose2[G, Z]] =
        F.map2K(fga)(fgb)([X] => (ga: G[A[X]], gb: G[B[X]]) => G.map2(ga, gb)((a, b) => f(a, b)))
  }

  given composeCatsInsideRight[F[_[_]], G[_]](using F: ApplyKC[F]): ApplyKC[[H[_]] =>> F[Compose2[H, G]]] with {
    extension [A[_], C](fag: F[Compose2[A, G]])
      override def mapK[B[_]](f: A :~>: B): F[Compose2[B, G]] = F.mapK(fag)([X] => (ag: A[G[X]]) => f(ag))

      override def map2K[B[_], Z[_]](fbg: F[Compose2[B, G]])(f: [X] => (A[X], B[X]) => Z[X]): F[Compose2[Z, G]] =
        F.map2K(fag)(fbg)([X] => (ag: A[G[X]], bg: B[G[X]]) => f(ag, bg))
  }

/**
  * A version of [[ApplyK]] without a normal type as well as a higher kinded
  * type.
  */
type ApplyKC[F[_[_]]] = ApplyK[IgnoreC[F]]
