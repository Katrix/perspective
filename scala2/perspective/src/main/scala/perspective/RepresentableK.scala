package perspective

import cats.Functor
import simulacrum.typeclass

@typeclass trait RepresentableK[F[_[_], _]] extends MonadK[F] with DistributiveK[F] {

  type RepresentationK[_]

  def indexK[A[_], C](fa: F[A, C]): RepresentationK ~>: A

  def tabulateK[A[_], C](f: RepresentationK ~>: A): F[A, C]
  
  def indices[C]: F[RepresentationK, C] =
    tabulateK(FunctionK.identity)

  override def pureK[A[_], C](a: Unit #~>: A): F[A, C] = tabulateK(λ[RepresentationK ~>: A](_ => a(())))

  override def flatMapK[A[_], B[_], C](fa: F[A, C])(f: A ~>: F[B, *]): F[B, C] =
    tabulateK(λ[RepresentationK ~>: B](a => indexK(f(indexK(fa)(a)))(a)))

  override def cosequenceK[G[_], A[_], C](gfa: G[F[A, C]])(implicit G: Functor[G]): F[Compose2[G, A, *], C] =
    tabulateK[Compose2[G, A, *], C](λ[RepresentationK ~>: Compose2[G, A, *]](r => G.map(gfa)(indexK(_)(r))))

  override def mapK[A[_], B[_], C](fa: F[A, C])(f: A ~>: B): F[B, C] =
    tabulateK(λ[RepresentationK ~>: B](r => f(indexK(fa)(r))))

  override def map2K[A[_], B[_], Z[_], C](fa: F[A, C], fb: F[B, C])(f: Tuple2K[A, B, *] ~>: Z): F[Z, C] =
    tabulateK(λ[RepresentationK ~>: Z](r => f((indexK(fa)(r), indexK(fb)(r)))))
}
object RepresentableK {
  type Aux[F[_[_], _], RepresentationK0[_]] = RepresentableK[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }
}
