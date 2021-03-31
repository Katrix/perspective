package perspective

import cats._
import cats.syntax.all._

import scala.language.implicitConversions

trait DistributiveK[F[_[_], _]] extends FunctorK[F]:
  extension[G[_]: Functor, A[_], C](gfa: G[F[A, C]]) 
    def distributeK[B[_]](f: Compose2[G, A] ~>: B): F[B, C] =
      gfa.cosequenceK.mapK(f)
    
    def distributeConst[B](f: Compose2[G, A] ~>#: B): F[Const[B], C] =
      //distributeK[Const[B]](f) //TODO: Doesn't work
      this.distributeK[G, A, C](gfa)[Const[B]](f)

    def cosequenceK: F[Compose2[G, A], C]

  extension[G[_]: Functor, A](ga: G[A]) def collectK[B[_], C](f: A => F[B, C]): F[Compose2[G, B], C] =
    ga.map(f).cosequenceK

type DistributiveKC[F[_[_]]] = DistributiveK[IgnoreC[F]]