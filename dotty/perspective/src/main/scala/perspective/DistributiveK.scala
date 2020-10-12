package perspective

import cats._
import cats.syntax.all._

import scala.language.implicitConversions

trait DistributiveK[F[_[_], _]] extends FunctorK[F]:
  extension[G[_]: Functor, A[_], B[_], C](gfa: G[F[A, C]]) def distributeK(f: Compose2[G, A] ~>: B): F[B, C] =
    gfa.cosequenceK.mapK(f)
  
  extension[G[_]: Functor, A, B[_], C](ga: G[A]) def collectK(f: A => F[B, C]): F[Compose2[G, B], C] =
    ga.map(f).cosequenceK
  
  extension[G[_]: Functor, A[_], C](gfa: G[F[A, C]]) def cosequenceK: F[Compose2[G, A], C]

type DistributiveKC[F[_[_]]] = DistributiveK[IgnoreC[F]]