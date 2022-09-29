package perspective.syntax

import scala.language.implicitConversions

import perspective.{Const, MonadK, MonadKC, ~>:}

trait MonadKSyntax {

  implicit def perspectiveMonadKCFAOps[F[_[_]]: MonadKC, A[_]](fa: F[A]): MonadKCFAOps[F, A] =
    new MonadKCFAOps[F, A](fa)

  implicit def perspectiveMonadKCFConstFAOps[F[_[_]]: MonadKC, A[_]](fa: F[Const[F[A], *]]): MonadKCFConstFAOps[F, A] =
    new MonadKCFConstFAOps[F, A](fa)

  implicit def perspectiveMonadKFAOps[F[_[_], _]: MonadK, A[_], C](fa: F[A, C]): MonadKFAOps[F, A, C] =
    new MonadKFAOps[F, A, C](fa)

  implicit def perspectiveMonadKFConstFAOps[F[_[_], _]: MonadK, A[_], C](
      fa: F[F[A, *], C]
  ): MonadKFConstFAOps[F, A, C] =
    new MonadKFConstFAOps[F, A, C](fa)
}

final class MonadKCFAOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def flatMapKC[B[_]](f: A ~>: Const[F[B], *])(implicit F: MonadKC[F]): F[B] =
    F.flatMapK(fa)(f)
}

final class MonadKCFConstFAOps[F[_[_]], A[_]](private val fa: F[Const[F[A], *]]) extends AnyVal {

  def flattenKC(implicit F: MonadKC[F]): F[A] = F.flattenK(fa)
}

final class MonadKFAOps[F[_[_], _], A[_], C](private val fa: F[A, C]) extends AnyVal {

  def flatMapKC[B[_]](f: A ~>: F[B, *])(implicit F: MonadK[F]): F[B, C] =
    F.flatMapK(fa)(f)
}

final class MonadKFConstFAOps[F[_[_], _], A[_], C](private val fa: F[F[A, *], C]) extends AnyVal {

  def flattenKC(implicit F: MonadK[F]): F[A, C] = F.flattenK(fa)
}
