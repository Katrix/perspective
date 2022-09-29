package perspective
package syntax

import scala.language.implicitConversions

import cats.kernel.Monoid

trait FoldableKSyntax {

  implicit def perspectiveFoldableKCFOps[F[_[_]]: FoldableKC, A[_]](fa: F[A]): FoldableKCOps[F, A] =
    new FoldableKCOps[F, A](fa)

  implicit def perspectiveFoldableConstKCFOps[F[_[_]]: FoldableKC, A](fa: F[Const[A, *]]): FoldableConstKCOps[F, A] =
    new FoldableConstKCOps[F, A](fa)

  implicit def perspectiveFoldableKFOps[F[_[_], _]: FoldableK, A[_], C](fa: F[A, C]): FoldableKOps[F, A, C] =
    new FoldableKOps[F, A, C](fa)

  implicit def perspectiveFoldableConstKFOps[F[_[_], _]: FoldableK, A, C](
      fa: F[Const[A, *], C]
  ): FoldableConstKOps[F, A, C] =
    new FoldableConstKOps[F, A, C](fa)
}

final class FoldableKCOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def foldLeftKC[B](b: B)(f: B => A ~>#: B)(implicit F: FoldableKC[F]): B = F.foldLeftK(fa, b)(f)

  def foldMapKC[B](f: A ~>#: B)(implicit F: FoldableKC[F], B: Monoid[B]): B = F.foldMapK(fa)(f)
}

final class FoldableConstKCOps[F[_[_]], A](private val fa: F[Const[A, *]]) extends AnyVal {

  def toListKC(implicit F: FoldableKC[F]): List[A] = F.toListK(fa)
}

final class FoldableKOps[F[_[_], _], A[_], C](private val fa: F[A, C]) extends AnyVal {

  def foldLeftKC[B](b: B)(f: B => A ~>#: B)(implicit F: FoldableK[F]): B = F.foldLeftK(fa, b)(f)

  def foldMapKC[B](f: A ~>#: B)(implicit F: FoldableK[F], B: Monoid[B]): B = F.foldMapK(fa)(f)
}

final class FoldableConstKOps[F[_[_], _], A, C](private val fa: F[Const[A, *], C]) extends AnyVal {

  def toListKC(implicit F: FoldableK[F]): List[A] = F.toListK(fa)
}
