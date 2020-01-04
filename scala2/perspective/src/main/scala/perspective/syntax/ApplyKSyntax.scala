package perspective
package syntax

import scala.language.implicitConversions

trait ApplyKSyntax extends ApplyK.ToApplyKOps {

  implicit def squealApplyKCFOps[F[_[_]]: ApplyKC, A[_]](fa: F[A]): ApplyKCOps[F, A] = new ApplyKCOps[F, A](fa)
}

final class ApplyKCOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def productKC[B[_]](fb: F[B])(implicit F: ApplyKC[F]): F[Tuple2K[A, B]#λ] = F.tupledK(fa, fb)

  def map2KC[B[_], Z[_]](fb: F[B])(f: Tuple2K[A, B]#λ ~>: Z)(implicit F: ApplyKC[F]): F[Z] = F.map2K(fa, fb)(f)
}
