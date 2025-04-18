package perspective

import scala.language.implicitConversions

import cats._
import cats.syntax.all._

/** A higher kinded [[Distributive]] typeclass. */
trait DistributiveK[F[_[_], _]] extends FunctorK[F]:
  extension [G[_]: Functor, A[_], C](gfa: G[F[A, C]])
    /** A higher kinded equivalent of [[Distributive.distribute]]. */
    def distributeK[B[_]](f: Compose2[G, A] :~>: B): F[B, C] =
      gfa.cosequenceK.mapK(f)

    /** Helper function that calls [[distributeK]] with [[Const]]. */
    inline def distributeConst[B](f: Compose2[G, A] :~>#: B): F[Const[B], C] =
      distributeK[Const[B]](f)

    /** A higher kinded equivalent of [[Distributive.cosequence]]. */
    def cosequenceK: F[Compose2[G, A], C]

  extension [G[_]: Functor, A](ga: G[A])
    def collectK[B[_], C](f: A => F[B, C]): F[Compose2[G, B], C] =
      ga.map(f).cosequenceK

object DistributiveK:
  given idInstanceC[A]: DistributiveKC[IdFC[A]] = perspective.instances.idInstanceC[A]

  given composeCatsOutside[F[_], G[_[_]]](
      using F: Distributive[F],
      G: DistributiveKC[G]
  ): DistributiveKC[[H[_]] =>> F[G[H]]] with
    extension [A[_], C](fa: F[G[A]]) override def mapK[B[_]](f: A :~>: B): F[G[B]] = fa.map(_.mapK(f))

    extension [H[_]: Functor, A[_], C](gfa: H[F[G[A]]])
      override def cosequenceK: F[G[Compose2[H, A]]] =
        F.cosequence(gfa).map(v => G.cosequenceK(v))

  given composeId[F[_], X](using F: Distributive[F]): DistributiveKC[[H[_]] =>> F[H[X]]] =
    composeCatsOutside[F, IdFC[X]]

  given composeCatsInside[F[_[_]], G[_]](
      using F: DistributiveKC[F],
      G: Distributive[G]
  ): DistributiveKC[[H[_]] =>> F[Compose2[G, H]]] with {
    extension [A[_], C](fga: F[Compose2[G, A]])
      override def mapK[B[_]](f: A :~>: B): F[Compose2[G, B]] = F.mapK(fga)([X] => (ga: G[A[X]]) => ga.map(a => f(a)))

    extension [H[_]: Functor, A[_], C](hfga: H[F[Compose2[G, A]]])
      override def cosequenceK: F[Compose3[G, H, A]] =
        val fhga: F[Compose3[H, G, A]] = F.cosequenceK(hfga)
        F.mapK(fhga)([X] => (hga: H[G[A[X]]]) => G.cosequence[H, A[X]](hga))

  }

  given composeCatsInsideRight[F[_[_]], G[_]](
    using F: DistributiveKC[F],
  ): DistributiveKC[[H[_]] =>> F[Compose2[H, G]]] with {
    extension [A[_], C](fag: F[Compose2[A, G]])
      override def mapK[B[_]](f: A :~>: B): F[Compose2[B, G]] = F.mapK(fag)([X] => (ag: A[G[X]]) => f(ag))

    extension [H[_] : Functor, A[_], C](hfag: H[F[Compose2[A, G]]])
      override def cosequenceK: F[Compose3[H, A, G]] = F.cosequenceK(hfag)

  }

/**
  * A version of [[DistributiveK]] without a normal type as well as a higher
  * kinded type.
  */
type DistributiveKC[F[_[_]]] = DistributiveK[IgnoreC[F]]
