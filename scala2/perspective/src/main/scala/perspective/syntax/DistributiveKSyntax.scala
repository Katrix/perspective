package perspective
package syntax

import cats.{Functor, Monad}

import scala.language.implicitConversions

trait DistributiveKSyntax extends DistributiveK.ToDistributiveKOps {

  implicit def perspectiveDistributiveKGFunctorOps[G[_]: Functor, A](ga: G[A]): DistributiveKCGFunctorOps[G, A] =
    new DistributiveKCGFunctorOps[G, A](ga)

  implicit def perspectiveDistributiveKGFAFunctorOps[G[_]: Functor, F[_[_], _]: DistributiveK, A[_], C](
      gfa: G[F[A, C]]
  ): DistributiveKGFAFunctorOps[G, F, A, C] =
    new DistributiveKGFAFunctorOps[G, F, A, C](gfa)

  implicit def perspectiveDistributiveKGFGMonadOps[G[_]: Monad, F[_[_], _]: DistributiveK, C](
      gfa: G[F[G, C]]
  ): DistributiveKGFGMonadOps[G, F, C] =
    new DistributiveKGFGMonadOps[G, F, C](gfa)

  implicit def perspectiveDistributiveKGFFunctorOps[G[_]: Functor, F[_[_], _]: DistributiveK, C](
      gf: G[F[cats.Id, C]]
  ): DistributiveKGFFunctorOps[G, F, C] =
    new DistributiveKGFFunctorOps[G, F, C](gf)

  implicit def perspectiveDistributiveKCGFAFunctorOps[G[_]: Functor, F[_[_]]: DistributiveKC, A[_]](
      gfa: G[F[A]]
  ): DistributiveKCGFAFunctorOps[G, F, A] =
    new DistributiveKCGFAFunctorOps[G, F, A](gfa)

  implicit def perspectiveDistributiveKCGFGMonadOps[G[_]: Monad, F[_[_]]: DistributiveKC](
      gfa: G[F[G]]
  ): DistributiveKCGFGMonadOps[G, F] =
    new DistributiveKCGFGMonadOps[G, F](gfa)

  implicit def perspectiveDistributiveKCGFFunctorOps[G[_]: Functor, F[_[_]]: DistributiveKC](
      gfa: G[F[cats.Id]]
  ): DistributiveKCGFFunctorOps[G, F] =
    new DistributiveKCGFFunctorOps[G, F](gfa)
}

final class DistributiveKCGFunctorOps[G[_], A](private val ga: G[A]) extends AnyVal {

  def collectK[F[_[_], _], B[_], C](
      f: A => F[B, C]
  )(implicit F: DistributiveK[F], G: Functor[G]): F[Compose2[G, B, *], C] =
    F.collectK(ga)(f)

  def collectKC[F[_[_]], B[_]](f: A => F[B])(implicit F: DistributiveKC[F], G: Functor[G]): F[Compose2[G, B, *]] =
    F.collectK(ga)(f)

  def collectIdK[F[_[_], _], C](
      f: A => F[cats.Id, C]
  )(implicit F: DistributiveK[F], G: Functor[G]): F[G, C] =
    F.collectIdK(ga)(f)

  def collectIdKC[F[_[_]]](f: A => F[cats.Id])(implicit F: DistributiveKC[F], G: Functor[G]): F[G] =
    F.collectIdK(ga)(f)
}

final class DistributiveKGFAFunctorOps[G[_], F[_[_], _], A[_], C](private val gfa: G[F[A, C]]) extends AnyVal {

  def distributeK[B[_]](f: Compose2[G, A, *] ~>: B)(implicit F: DistributiveK[F], G: Functor[G]): F[B, C] =
    F.distributeK(gfa)(f)

  def cosequenceK(implicit F: DistributiveK[F], G: Functor[G]): F[Compose2[G, A, *], C] = F.cosequenceK(gfa)
}

final class DistributiveKGFGMonadOps[G[_], F[_[_], _], C](private val gfg: G[F[G, C]]) extends AnyVal {

  def distributeFlattenK(implicit F: DistributiveK[F], G: Monad[G]): F[G, C] = F.distributeFlattenK(gfg)
}

final class DistributiveKGFFunctorOps[G[_], F[_[_], _], C](private val gfa: G[F[cats.Id, C]]) extends AnyVal {

  def distributeIdK[B[_]](f: G ~>: B)(implicit F: DistributiveK[F], G: Functor[G]): F[B, C] =
    F.distributeIdK(gfa)(f)

  def cosequenceIdK(implicit F: DistributiveK[F], G: Functor[G]): F[G, C] = F.cosequenceIdK(gfa)
}

final class DistributiveKCGFAFunctorOps[G[_], F[_[_]], A[_]](private val gfa: G[F[A]]) extends AnyVal {

  def distributeKC[B[_]](f: Compose2[G, A, *] ~>: B)(implicit F: DistributiveKC[F], G: Functor[G]): F[B] =
    F.distributeK(gfa)(f)

  def cosequenceKC(implicit F: DistributiveKC[F], G: Functor[G]): F[Compose2[G, A, *]] = F.cosequenceK(gfa)
}

final class DistributiveKCGFGMonadOps[G[_], F[_[_]]](private val gfg: G[F[G]]) extends AnyVal {

  def distributeFlattenKC(implicit F: DistributiveKC[F], G: Monad[G]): F[G] = F.distributeFlattenK(gfg)
}

final class DistributiveKCGFFunctorOps[G[_], F[_[_]]](private val gfa: G[F[cats.Id]]) extends AnyVal {

  def distributeIdKC[B[_]](f: G ~>: B)(implicit F: DistributiveKC[F], G: Functor[G]): F[B] =
    F.distributeIdK(gfa)(f)

  def cosequenceIdKC(implicit F: DistributiveKC[F], G: Functor[G]): F[G] = F.cosequenceIdK(gfa)
}
