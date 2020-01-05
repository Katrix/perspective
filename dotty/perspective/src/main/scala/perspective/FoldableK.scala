package perspective

import cats._
import cats.syntax.all._

import scala.language.implicitConversions

trait FoldableK[F[_[_], _]]
  def [A[_], B, C](fa: F[A, C]) foldLeftK(b: B)(f: B => A ~>#: B): B

  def [A[_], B, C] (fa: F[A, C]) foldMapK(f: A ~>#: B)(given B: Monoid[B]): B =
    fa.foldLeftK(B.empty)(b => [Z] => (az: A[Z]) => b.combine(f(az)))

type FoldableKC[F[_[_]]] = FoldableK[IgnoreC[F]]