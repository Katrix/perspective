package perspective

import cats._
import cats.syntax.all._

trait TraverseK[F[_[_], _]] extends FunctorK[F], FoldableK[F]:
  extension[G[_]: Applicative, A[_], B[_], C](fa: F[A, C]) def traverseK(f: A ~>: Compose2[G, B]): G[F[B, C]]

  extension[G[_]: Applicative, A[_], C](fga: F[Compose2[G, A], C]) def sequenceK: G[F[A, C]] =
    fga.traverseK(FunctionK.identity[Compose2[G, A]])
  
  extension [A[_], B[_], C](fa: F[A, C]) override def mapK(f: A ~>: B): F[B, C] =
    fa.traverseK[Id, A, B, C](f)

  extension[G[_]: Applicative, A[_], C](fa: F[A, C]) def traverseIdK (f: A ~>: G): G[F[Id, C]] = 
    fa.traverseK[G, A, Id, C](f)
  
  extension [A[_]: Applicative, C](fa: F[A, C]) def sequenceIdK: A[F[Id, C]] =
    fa.sequenceK[A, Id, C]

type TraverseKC[F[_[_]]] = TraverseK[IgnoreC[F]]