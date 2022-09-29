package perspective

import scala.language.implicitConversions

import cats._
import cats.syntax.all._

/** A higher kinded [[Distributive]] typeclass. */
trait DistributiveK[F[_[_], _]] extends FunctorK[F]:
  extension [G[_]: Functor, A[_], C](gfa: G[F[A, C]])
    /** A higher kinded equivalent of [[Distributive.distribute]]. */
    def distributeK[B[_]](f: Compose2[G, A] ~>: B): F[B, C] =
      gfa.cosequenceK.mapK(f)

    /** Helper function that calls [[distributeK]] with [[Const]]. */
    inline def distributeConst[B](f: Compose2[G, A] ~>#: B): F[Const[B], C] =
      distributeK[Const[B]](f)

    /** A higher kinded equivalent of [[Distributive.cosequence]]. */
    def cosequenceK: F[Compose2[G, A], C]

  extension [G[_]: Functor, A](ga: G[A])
    def collectK[B[_], C](f: A => F[B, C]): F[Compose2[G, B], C] =
      ga.map(f).cosequenceK

/**
  * A version of [[DistributiveK]] without a normal type as well as a higher
  * kinded type.
  */
type DistributiveKC[F[_[_]]] = DistributiveK[IgnoreC[F]]
