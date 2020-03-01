package perspective.syntax

import perspective.{Const, MonadK, MonadKC, ~>:}

import scala.language.implicitConversions

trait MonadKSyntax extends MonadK.ToMonadKOps {

  implicit def perspectiveMonadKCFAOps[F[_[_]]: MonadKC, A[_]](fa: F[A]): MonadKCFAOps[F, A] =
    new MonadKCFAOps[F, A](fa)

  implicit def perspectiveMonadKCFConstFAOps[F[_[_]]: MonadKC, A[_]](fa: F[Const[F[A], *]]): MonadKCFConstFAOps[F, A] =
    new MonadKCFConstFAOps[F, A](fa)
}

final class MonadKCFAOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def flatMapKC[B[_]](f: A ~>: Const[F[B], *])(implicit F: MonadKC[F]): F[B] =
    F.flatMapK(fa)(f)
}

final class MonadKCFConstFAOps[F[_[_]], A[_]](private val fa: F[Const[F[A], *]]) extends AnyVal {

  def flattenKC(implicit F: MonadKC[F]): F[A] = F.flattenK(fa)
}
