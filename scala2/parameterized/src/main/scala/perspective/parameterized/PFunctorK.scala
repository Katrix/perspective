package perspective.parameterized

import cats.Functor
import simulacrum.typeclass

@typeclass trait PFunctorK[F[_[_], _]] {

  def mapK[A[_]: Functor, B[_]: Functor, C](fa: F[A, C])(f: A ~>: B): F[B, C]

  def liftK[A[_]: Functor, B[_]: Functor](f: A ~>: B): F[A, *] ~>: F[B, *] =
    λ[F[A, *] ~>: F[B, *]](fa => mapK(fa)(f))

  def voidK[A[_]: Functor, C](fa: F[A, C]): F[Const[Unit, *], C] = asK(fa, FunctionK.const(()))

  def asK[A[_]: Functor, B[_]: Functor, C](fa: F[A, C], b: Const[Unit, *] ~>: B): F[B, C] =
    mapK(fa)(Lambda[A ~>: B](_ => b(Const.unit)))

  def asConstK[A[_]: Functor, B, C](fa: F[A, C], b: B): F[Const[B, *], C] =
    asK(fa, FunctionK.const(b))

  def widen[A[_], B[D] >: A[D], C](fa: F[A, C]): F[B, C] = fa.asInstanceOf[F[B, C]]
}
