package perspective
package syntax

import scala.language.implicitConversions

import cats.Applicative

trait TraverseKSyntax {

  implicit def perspectiveTraverseKCFAOps[F[_[_]]: TraverseKC, A[_]](
      fa: F[A]
  ): TraverseKCFAOps[F, A] =
    new TraverseKCFAOps[F, A](fa)

  implicit def perspectiveTraverseKCFComposeGAOps[F[_[_]]: TraverseKC, G[_], A[_]](
      fa: F[Compose2[G, A, *]]
  ): TraverseKCFComposeGAApplicativeOps[F, G, A] =
    new TraverseKCFComposeGAApplicativeOps[F, G, A](fa)

  implicit def perspectiveTraverseKFAOps[F[_[_], _]: TraverseK, A[_], C](
      fa: F[A, C]
  ): TraverseKFAOps[F, A, C] =
    new TraverseKFAOps[F, A, C](fa)

  implicit def perspectiveTraverseKFComposeGAOps[F[_[_], _]: TraverseK, G[_], A[_], C](
      fa: F[Compose2[G, A, *], C]
  ): TraverseKFComposeGAApplicativeOps[F, G, A, C] =
    new TraverseKFComposeGAApplicativeOps[F, G, A, C](fa)
}

final class TraverseKCFAOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def traverseKC[G[_], B[_]](
      f: A ~>: Compose2[G, B, *]
  )(implicit F: TraverseKC[F], G: Applicative[G]): G[F[B]] = F.traverseK(fa)(f)

  def traverseIdK[G[_]: Applicative](f: A ~>: G)(implicit F: TraverseKC[F]): G[F[cats.Id]] =
    F.traverseIdK(fa)(f)

  def sequenceIdKC(implicit F: TraverseKC[F], A: Applicative[A]): A[F[cats.Id]] =
    F.sequenceIdK(fa)
}

final class TraverseKCFComposeGAApplicativeOps[F[_[_]], G[_], A[_]](private val fga: F[Compose2[G, A, *]])
    extends AnyVal {

  def sequenceK(implicit F: TraverseKC[F], G: Applicative[G]): G[F[A]] =
    F.sequenceK(fga)
}

final class TraverseKFAOps[F[_[_], _], A[_], C](private val fa: F[A, C]) extends AnyVal {

  def traverseKC[G[_], B[_]](
      f: A ~>: Compose2[G, B, *]
  )(implicit F: TraverseK[F], G: Applicative[G]): G[F[B, C]] = F.traverseK(fa)(f)

  def traverseIdK[G[_]: Applicative](f: A ~>: G)(implicit F: TraverseK[F]): G[F[cats.Id, C]] =
    F.traverseIdK(fa)(f)

  def sequenceIdKC(implicit F: TraverseK[F], A: Applicative[A]): A[F[cats.Id, C]] =
    F.sequenceIdK(fa)
}

final class TraverseKFComposeGAApplicativeOps[F[_[_], _], G[_], A[_], C](private val fga: F[Compose2[G, A, *], C])
    extends AnyVal {

  def sequenceK(implicit F: TraverseK[F], G: Applicative[G]): G[F[A, C]] =
    F.sequenceK(fga)
}
