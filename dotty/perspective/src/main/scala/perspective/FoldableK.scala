package perspective

import scala.language.implicitConversions

import cats._
import cats.syntax.all._

/** A higher kinded [[Foldable]] typeclass. */
trait FoldableK[F[_[_], _]]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[Foldable.foldLeft]]. */
    def foldLeftK[B](b: B)(f: B => A :~>#: B): B

    def foldRightK[B](b: B)(f: A :~>#: (B => B)): B

    /** A higher kinded equivalent of [[Foldable.foldMap]]. */
    def foldMapK[B](f: A :~>#: B)(using B: Monoid[B]): B =
      foldLeftK(B.empty)(b => [Z] => (az: A[Z]) => b.combine(f(az)))

  /** A higher kinded equivalent of [[Foldable.toList]]. */
  extension [A, C](fa: F[Const[A], C])
    def toListK: List[A] =
      fa.foldMapK(FunctionK.liftConst(List(_: A)))

object FoldableK:
  given idInstanceC[A]: FoldableKC[IdFC[A]] = perspective.instances.idInstanceC[A]

  given composeCats[F[_], G[_[_]]](using F: Foldable[F], G: FoldableKC[G]): FoldableKC[[H[_]] =>> F[G[H]]] with {
    extension [A[_], C](fa: F[G[A]])
      override def foldLeftK[B](b: B)(f: B => A :~>#: B): B =
        fa.foldLeft(b)((bacc, a) => a.foldLeftK(bacc)(f))

      override def foldRightK[B](b: B)(f: A :~>#: (B => B)): B =
        fa.foldRight(Eval.now(b))((a, bacce) => Eval.now(a.foldRightK(bacce.value)(f))).value
  }

  given composeId[F[_], X](using F: Foldable[F]): FoldableKC[[H[_]] =>> F[H[X]]] = composeCats[F, IdFC[X]]

type FoldableKC[F[_[_]]] = FoldableK[IgnoreC[F]]
