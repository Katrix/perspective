package perspective

import cats._
import cats.syntax.all._

import scala.language.implicitConversions

trait DistributiveK[F[_[_], _]] extends FunctorK[F]:
  def [G[_]: Functor, A[_], B[_], C](gfa: G[F[A, C]]) distributeK(f: Compose2[G, A] ~>: B): F[B, C] =
    gfa.cosequenceK.mapK(f)
  
  def [G[_]: Functor, A, B[_], C](ga: G[A]) collectK(f: A => F[B, C]): F[Compose2[G, B], C] =
    ga.map(f).cosequenceK
  
  def [G[_]: Functor, A[_], C](gfa: G[F[A, C]]) cosequenceK: F[Compose2[G, A], C]

type DistributiveKC[F[_[_]]] = DistributiveK[IgnoreC[F]]