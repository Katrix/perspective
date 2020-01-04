package perspective
package syntax

import scala.language.implicitConversions

trait FunctorKSyntax extends FunctorK.ToFunctorKOps {

  implicit def squealFunctorKCFOps[F[_[_]]: FunctorKC, A[_]](fa: F[A]): FunctorKCOps[F, A] = new FunctorKCOps[F, A](fa)
}

final class FunctorKCOps[F[_[_]], A[_]](private val fa: F[A]) extends AnyVal {

  def mapKC[B[_]](f: A ~>: B)(implicit F: FunctorKC[F]): F[B] = F.mapK(fa)(f)

  def voidKC(implicit F: FunctorKC[F]): F[Const[Unit]#λ] = F.voidK(fa)

  def asKC[B[_]](b: Unit #~>: B)(implicit F: FunctorKC[F]): F[B] = F.asK(fa, b)
}
