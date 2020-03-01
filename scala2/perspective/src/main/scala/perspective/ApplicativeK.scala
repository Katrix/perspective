package perspective

import simulacrum.typeclass

@typeclass trait ApplicativeK[F[_[_], _]] extends ApplyK[F] {

  def pureK[A[_], C](a: Unit #~>: A): F[A, C]

  def pureConstK[A, C](a: A): F[Const[A, *], C] =
    pureK(FunctionK.liftConst(_ => a))

  def unitK[C]: F[Const[Unit, *], C] = pureConstK(())

  override def mapK[A[_], B[_], C](fa: F[A, C])(f: A ~>: B): F[B, C] =
    apK(pureK[λ[D => A[D] => B[D]], C](λ[Const[Unit, *] ~>: λ[D => A[D] => B[D]]](_ => f.apply)))(fa)
}
