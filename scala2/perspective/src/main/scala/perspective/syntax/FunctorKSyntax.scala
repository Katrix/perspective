package perspective
package syntax

import scala.language.implicitConversions

trait FunctorKSyntax {

  implicit def perspectiveFunctorKCFOps[F[_[_]]: FunctorKC, A[_]](fa: F[A]): FunctorKCOps[F, A] =
    new FunctorKCOps[F, A](fa)

  implicit def perspectiveFunctorKFOps[F[_[_], _] : FunctorK, A[_], C](fa: F[A, C]): FunctorKOps[F, A, C] =
    new FunctorKOps[F, A, C](fa)
}

final class FunctorKCOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def mapKC[B[_]](f: A ~>: B)(implicit F: FunctorKC[F]): F[B] = F.mapK(fa)(f)

  def voidKC(implicit F: FunctorKC[F]): F[Const[Unit, *]] = F.voidK(fa)

  def asKC[B[_]](b: Unit #~>: B)(implicit F: FunctorKC[F]): F[B] = F.asK(fa, b)
}

final class FunctorKOps[F[_[_], _], A[_], C](private val fa: F[A, C]) extends AnyVal {

  def mapK[B[_]](f: A ~>: B)(implicit F: FunctorK[F]): F[B, C] = F.mapK(fa)(f)

  def voidK(implicit F: FunctorK[F]): F[Const[Unit, *], C] = F.voidK(fa)

  def asK[B[_]](b: Unit #~>: B)(implicit F: FunctorK[F]): F[B, C] = F.asK(fa, b)
}
