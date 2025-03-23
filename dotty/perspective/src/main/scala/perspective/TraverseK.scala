package perspective

import cats._
import cats.syntax.all._

/** A higher kinded [[Traverse]] typeclass. */
trait TraverseK[F[_[_], _]] extends FunctorK[F], FoldableK[F]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[Traverse.traverse]]. */
    def traverseK[G[_]: Applicative, B[_]](f: A :~>: Compose2[G, B]): G[F[B, C]]

    /** Helper function that calls [[traverseK]] with [[Const]]. */
    inline def traverseConst[G[_]: Applicative, B](f: A :~>#: G[B]): G[F[Const[B], C]] =
      traverseK(f)

    /** Helper function that calls [[traverseK]] with [[Id]]. */
    inline def traverseIdK[G[_]: Applicative](f: A :~>: G): G[F[Id, C]] =
      traverseK(f)

    /** Helper function that calls [[sequenceK]] with [[Id]]. */
    inline def sequenceIdK(using Applicative[A]): A[F[Id, C]] =
      fa.sequenceK

    override def mapK[B[_]](f: A :~>: B): F[B, C] =
      traverseK[Id, B](f)

  extension [G[_]: Applicative, A[_], C](fga: F[Compose2[G, A], C])
    /** A higher kinded equivalent of [[Traverse.sequence]]. */
    def sequenceK: G[F[A, C]] =
      fga.traverseK(FunctionK.identity[Compose2[G, A]])

object TraverseK:
  given idInstanceC[A]: TraverseKC[IdFC[A]] = perspective.instances.idInstanceC[A]

  given composeCatsOutside[F[_], G[_[_]]](using F: Traverse[F], G: TraverseKC[G]): TraverseKC[[H[_]] =>> F[G[H]]] with {
    extension [A[_], C](fa: F[G[A]])
      override def mapK[B[_]](f: A :~>: B): F[G[B]] = fa.map(_.mapK(f))

      override def foldLeftK[B](b: B)(f: B => A :~>#: B): B =
        fa.foldLeft(b)((bacc, a) => a.foldLeftK(bacc)(f))

      override def foldRightK[B](b: B)(f: A :~>#: (B => B)): B =
        fa.foldRight(Eval.now(b))((a, bacce) => Eval.now(a.foldRightK(bacce.value)(f))).value

      override def traverseK[H[_]: Applicative, B[_]](f: A :~>: Compose2[H, B]): H[F[G[B]]] =
        fa.traverse(_.traverseK(f))
  }

  given composeId[F[_], X](using F: Traverse[F]): TraverseKC[[H[_]] =>> F[H[X]]] = composeCatsOutside[F, IdFC[X]]

  given composeCatsInside[F[_[_]], G[_]](
      using F: TraverseKC[F],
      G: Traverse[G]
  ): TraverseKC[[H[_]] =>> F[Compose2[G, H]]] with {
    extension [A[_], C](fga: F[Compose2[G, A]])
      override def foldLeftK[B](b: B)(f: B => A :~>#: B): B =
        F.foldLeftK(fga)(b)(bacc => [Z] => (ga: G[A[Z]]) => ga.foldLeft(bacc)((bacc2, a) => f(bacc2)(a)))

      override def foldRightK[B](b: B)(f: A :~>#: (B => B)): B =
        F.foldRightK(fga)(b)(
          [Z] =>
            (ga: G[A[Z]]) =>
              (bacc: B) => ga.foldRight(Eval.now(bacc))((a, bacc2) => bacc2.map(bacc3 => f(a)(bacc3))).value
        )

      def traverseK[H[_]: Applicative, B[_]](f: A :~>: Compose2[H, B]): H[F[Compose2[G, B]]] =
        F.traverseK(fga)[H, Compose2[G, B]]([Z] => (ga: G[A[Z]]) => G.traverse[H, A[Z], B[Z]](ga)(a => f(a)))
  }

  given composeCatsInsideRight[F[_[_]], G[_]](
      using F: TraverseKC[F]
  ): TraverseKC[[H[_]] =>> F[Compose2[H, G]]] with {
    extension [A[_], C](fag: F[Compose2[A, G]])
      override def foldLeftK[B](b: B)(f: B => A :~>#: B): B =
        F.foldLeftK(fag)(b)(bacc => [Z] => (ag: A[G[Z]]) => f(bacc)(ag))

      override def foldRightK[B](b: B)(f: A :~>#: (B => B)): B =
        F.foldRightK(fag)(b)([Z] => (ag: A[G[Z]]) => (bacc: B) => f(ag)(bacc))

      def traverseK[H[_]: Applicative, B[_]](f: A :~>: Compose2[H, B]): H[F[Compose2[B, G]]] =
        F.traverseK(fag)[H, Compose2[B, G]]([Z] => (ag: A[G[Z]]) => f(ag))
  }

/**
  * A version of [[TraverseK]] without a normal type as well as a higher kinded
  * type.
  */
type TraverseKC[F[_[_]]] = TraverseK[IgnoreC[F]]
