package perspective

import scala.language.implicitConversions

import cats._
import cats.syntax.all._

/** A higher kinded [[Foldable]] typeclass. */
trait FoldableK[F[_[_], _]]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[Foldable.foldLeft]]. */
    def foldLeftK[B](b: B)(f: B => A ~>#: B): B

    /** A higher kinded equivalent of [[Foldable.foldMap]]. */
    def foldMapK[B](f: A ~>#: B)(using B: Monoid[B]): B =
      foldLeftK(B.empty)(b => [Z] => (az: A[Z]) => b.combine(f(az)))

  /** A higher kinded equivalent of [[Foldable.toList]]. */
  extension [A, C](fa: F[Const[A], C])
    def toListK: List[A] =
      fa.foldMapK(FunctionK.liftConst(List(_: A)))

type FoldableKC[F[_[_]]] = FoldableK[IgnoreC[F]]
