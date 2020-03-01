package perspective
package syntax

import cats.kernel.Monoid

import scala.language.implicitConversions

trait FoldableKSyntax extends FoldableK.ToFoldableKOps {

  implicit def perspectiveFoldableKCFOps[F[_[_]]: FoldableKC, A[_]](fa: F[A]): FoldableKCOps[F, A] =
    new FoldableKCOps[F, A](fa)

  implicit def perspectiveFoldableConstKCFOps[F[_[_]]: FoldableKC, A](fa: F[Const[A, *]]): FoldableConstKCOps[F, A] =
    new FoldableConstKCOps[F, A](fa)
}

final class FoldableKCOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def foldLeftKC[B](b: B)(f: B => A ~>#: B)(implicit F: FoldableKC[F]): B = F.foldLeftK(fa, b)(f)

  def foldMapKC[B](f: A ~>#: B)(implicit F: FoldableKC[F], B: Monoid[B]): B = F.foldMapK(fa)(f)
}

final class FoldableConstKCOps[F[_[_]], A](private val fa: F[Const[A, *]]) extends AnyVal {

  def toListKC(implicit F: FoldableKC[F]): List[A] = F.toListK(fa)
}
