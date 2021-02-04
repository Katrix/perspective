package perspective

import cats._
import cats.syntax.all._

trait TraverseK[F[_[_], _]] extends FunctorK[F], FoldableK[F]:
  extension[A[_], C](fa: F[A, C]) 
    def traverseK[G[_]: Applicative, B[_]](f: A ~>: Compose2[G, B]): G[F[B, C]]

    def traverseIdK[G[_]: Applicative] (f: A ~>: G): G[F[Id, C]] =
      traverseK(f)

    def sequenceIdK(using Applicative[A]): A[F[Id, C]] =
      fa.sequenceK

    override def mapK[B[_]](f: A ~>: B): F[B, C] = 
      traverseK[Id, B](f)

  extension[G[_]: Applicative, A[_], C](fga: F[Compose2[G, A], C]) def sequenceK: G[F[A, C]] =
    fga.traverseK(FunctionK.identity[Compose2[G, A]])
  
type TraverseKC[F[_[_]]] = TraverseK[IgnoreC[F]]