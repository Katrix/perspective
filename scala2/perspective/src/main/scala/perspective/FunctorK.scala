package perspective

import simulacrum.typeclass

@typeclass trait FunctorK[F[_[_], _]] {

  def mapK[A[_], B[_], C](fa: F[A, C])(f: A ~>: B): F[B, C]

  def liftK[A[_], B[_]](f: A ~>: B): F[A, *] ~>: F[B, *] =
    λ[F[A, *] ~>: F[B, *]](fa => mapK(fa)(f))

  def voidK[A[_], C](fa: F[A, C]): F[Const[Unit, *], C] = asK(fa, FunctionK.const(()))

  def asK[A[_], B[_], C](fa: F[A, C], b: Unit #~>: B): F[B, C] = mapK(fa)(Lambda[A ~>: B](_ => b(())))

  def asConstK[A[_], B, C](fa: F[A, C], b: B): F[Const[B, *], C] =
    asK(fa, FunctionK.const(b))

  def widen[A[_], B[D] >: A[D], C](fa: F[A, C]): F[B, C] = fa.asInstanceOf[F[B, C]]
}
