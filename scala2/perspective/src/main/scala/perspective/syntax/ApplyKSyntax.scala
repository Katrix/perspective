package perspective
package syntax

import scala.language.implicitConversions

trait ApplyKSyntax {

  implicit def perspectiveApplyKCFOps[F[_[_]]: ApplyKC, A[_]](fa: F[A]): ApplyKCOps[F, A] = new ApplyKCOps[F, A](fa)

  implicit def perspectiveApplyKFOps[F[_[_], _]: ApplyK, A[_], C](fa: F[A, C]): ApplyKOps[F, A, C] = new ApplyKOps[F, A, C](fa)
}

final class ApplyKCOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def productKC[B[_]](fb: F[B])(implicit F: ApplyKC[F]): F[Tuple2K[A, B, *]] = F.tupledK(fa, fb)

  def map2KC[B[_], Z[_]](fb: F[B])(f: Tuple2K[A, B, *] ~>: Z)(implicit F: ApplyKC[F]): F[Z] = F.map2K(fa, fb)(f)
}

final class ApplyKOps[F[_[_], _], A[_], C](private val fa: F[A, C]) extends AnyVal {

  def productK[B[_]](fb: F[B, C])(implicit F: ApplyK[F]): F[Tuple2K[A, B, *], C] = F.tupledK(fa, fb)

  def map2K[B[_], Z[_]](fb: F[B, C])(f: Tuple2K[A, B, *] ~>: Z)(implicit F: ApplyK[F]): F[Z, C] = F.map2K(fa, fb)(f)
}
