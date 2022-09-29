package perspective

import cats._
import cats.syntax.all._

/** A higher kinded [[Traverse]] typeclass. */
trait TraverseK[F[_[_], _]] extends FunctorK[F], FoldableK[F]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[Traverse.traverse]]. */
    def traverseK[G[_]: Applicative, B[_]](f: A ~>: Compose2[G, B]): G[F[B, C]]

    /** Helper function that calls [[traverseK]] with [[Const]]. */
    inline def traverseConst[G[_]: Applicative, B](f: A ~>#: G[B]): G[F[Const[B], C]] =
      traverseK(f)

    /** Helper function that calls [[traverseK]] with [[Id]]. */
    inline def traverseIdK[G[_]: Applicative](f: A ~>: G): G[F[Id, C]] =
      traverseK(f)

    /** Helper function that calls [[sequenceK]] with [[Id]]. */
    inline def sequenceIdK(using Applicative[A]): A[F[Id, C]] =
      fa.sequenceK

    override def mapK[B[_]](f: A ~>: B): F[B, C] =
      traverseK[Id, B](f)

  extension [G[_]: Applicative, A[_], C](fga: F[Compose2[G, A], C])
    /** A higher kinded equivalent of [[Traverse.sequence]]. */
    def sequenceK: G[F[A, C]] =
      fga.traverseK(FunctionK.identity[Compose2[G, A]])

/**
  * A version of [[TraverseK]] without a normal type as well as a higher kinded
  * type.
  */
type TraverseKC[F[_[_]]] = TraverseK[IgnoreC[F]]
