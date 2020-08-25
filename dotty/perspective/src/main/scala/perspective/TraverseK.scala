package perspective

import cats._
import cats.syntax.all._

trait TraverseK[F[_[_], _]] extends FunctorK[F], FoldableK[F]:
  def [G[_]: Applicative, A[_], B[_], C](fa: F[A, C]) traverseK(f: A ~>: Compose2[G, B]): G[F[B, C]]

  def [G[_]: Applicative, A[_], C](fga: F[Compose2[G, A], C]) sequenceK: G[F[A, C]] =
    fga.traverseK(FunctionK.identity[Compose2[G, A]])
  
  override def [A[_], B[_], C](fa: F[A, C]) mapK(f: A ~>: B): F[B, C] =
    fa.traverseK[Id, A, B, C](f)

  def [G[_]: Applicative, A[_], C](fa: F[A, C]) traverseIdK (f: A ~>: G): G[F[Id, C]] = 
    fa.traverseK[G, A, Id, C](f)
  
  def [A[_]: Applicative, C](fa: F[A, C]) sequenceIdK: A[F[Id, C]] =
    fa.sequenceK[A, Id, C]

type TraverseKC[F[_[_]]] = TraverseK[IgnoreC[F]]