package perspective

import cats.{Functor, Monad}

trait DistributiveK[F[_[_], _]] extends FunctorK[F] {

  def distributeK[G[_]: Functor, A[_], B[_], C](gfa: G[F[A, C]])(f: Compose2[G, A, *] ~>: B): F[B, C] =
    mapK(cosequenceK(gfa))(f)

  def collectK[G[_]: Functor, A, B[_], C](ga: G[A])(f: A => F[B, C]): F[Compose2[G, B, *], C] =
    cosequenceK(Functor[G].map(ga)(f))

  def cosequenceK[G[_]: Functor, A[_], C](gfa: G[F[A, C]]): F[Compose2[G, A, *], C]

  def distributeFlattenK[G[_]: Monad, C](gfg: G[F[G, C]]): F[G, C] =
    distributeK(gfg)(λ[Compose2[G, G, *] ~>: G](Monad[G].flatten(_)))

  def distributeIdK[G[_]: Functor, B[_], C](gf: G[F[cats.Id, C]])(f: G ~>: B): F[B, C] =
    distributeK[G, cats.Id, B, C](gf)(f)

  def collectIdK[G[_]: Functor, A, C](ga: G[A])(f: A => F[cats.Id, C]): F[G, C] =
    collectK[G, A, cats.Id, C](ga)(f)

  def cosequenceIdK[G[_]: Functor, C](gf: G[F[cats.Id, C]]): F[G, C] = cosequenceK[G, cats.Id, C](gf)
}
