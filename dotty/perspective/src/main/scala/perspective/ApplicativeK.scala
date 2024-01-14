package perspective

import cats.Applicative
import cats.syntax.all.*

/** A higher kinded [[cats.Applicative]] typeclass. */
trait ApplicativeK[F[_[_], _]] extends ApplyK[F]:
  /** A higher kinded equivalent of [[cats.Applicative.pure]]. */
  extension [A[_]](a: ValueK[A]) def pure[C]: F[A, C]

  /** A higher kinded equivalent of [[cats.Applicative.unit]]. */
  def unitK[C]: F[Const[Unit], C] = ValueK.const(()).pure

  extension [A[_], C](fa: F[A, C])
    override def mapK[B[_]](f: A :~>: B): F[B, C] =
      this.pure[[Z] =>> A[Z] => B[Z]]([Z] => () => f[Z])[C].ap(fa)
object ApplicativeK:
  given idInstanceC[A]: ApplicativeKC[IdFC[A]] = instances.idInstanceC[A]

  given composeCatsOutside[F[_], G[_[_]]](
      using F: Applicative[F],
      G: ApplicativeKC[G]
  ): ApplicativeKC[[H[_]] =>> F[G[H]]] with {
    extension [A[_], C](fa: F[G[A]])
      override def map2K[B[_], Z[_]](fb: F[G[B]])(f: [Y] => (A[Y], B[Y]) => Z[Y]): F[G[Z]] =
        fa.map2(fb)((a, b) => a.map2K(b)(f))

      override def mapK[B[_]](f: A :~>: B): F[G[B]] = fa.map(_.mapK(f))

    extension [A[_]](a: ValueK[A]) override def pure[C]: F[G[A]] = F.pure(G.pure(a))
  }

  given composeId[F[_], X](using F: Applicative[F]): ApplicativeKC[[H[_]] =>> F[H[X]]] = composeCatsOutside[F, IdFC[X]]

  given composeCatsInside[F[_[_]], G[_]](
      using F: ApplicativeKC[F],
      G: Applicative[G]
  ): ApplicativeKC[[H[_]] =>> F[Compose2[G, H]]] with {
    extension [A[_], C](fga: F[Compose2[G, A]])
      override def mapK[B[_]](f: A :~>: B): F[Compose2[G, B]] = F.mapK(fga)([X] => (ga: G[A[X]]) => ga.map(a => f(a)))

      override def map2K[B[_], Z[_]](fgb: F[Compose2[G, B]])(f: [X] => (A[X], B[X]) => Z[X]): F[Compose2[G, Z]] =
        F.map2K(fga)(fgb)([X] => (ga: G[A[X]], gb: G[B[X]]) => G.map2(ga, gb)((a, b) => f(a, b)))

    extension [A[_]](a: ValueK[A]) override def pure[C]: F[Compose2[G, A]] = F.pure([X] => () => G.pure(a[X]()))
  }

/**
  * A version of [[ApplicativeK]] without a normal type as well as a higher
  * kinded type.
  */
type ApplicativeKC[F[_[_]]] = ApplicativeK[IgnoreC[F]]
