package perspective
package syntax

import cats.Applicative

import scala.language.implicitConversions

trait TraverseKSyntax extends TraverseK.ToTraverseKOps {

  implicit def perspectiveTraverseKCFAOps[F[_[_]]: TraverseKC, A[_]](
      fa: F[A]
  ): TraverseKCFAOps[F, A] =
    new TraverseKCFAOps[F, A](fa)

  implicit def perspectiveTraverseKCFComposeGAOps[F[_[_]]: TraverseKC, G[_], A[_]](
      fa: F[Compose2[G, A, *]]
  ): TraverseKCFComposeGAApplicativeOps[F, G, A] =
    new TraverseKCFComposeGAApplicativeOps[F, G, A](fa)
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
