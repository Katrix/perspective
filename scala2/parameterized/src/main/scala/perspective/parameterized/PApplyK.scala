package perspective.parameterized

import cats.Apply
import simulacrum.typeclass

@typeclass trait PApplyK[F[_[_], _]] extends PFunctorK[F] {

  def apK[A[_]: Apply, B[_]: Apply, C](ff: F[λ[D => A[D] => B[D]], C])(fa: F[A, C]): F[B, C]

  def tupledK[A[_]: Apply, B[_]: Apply, C](fa: F[A, C], fb: F[B, C]): F[Tuple2K[A, B, *], C] =
    map2K(fa, fb)(FunctionK.identity)

  def map2K[A[_]: Apply, B[_]: Apply, X[_]: Apply, C](fa: F[A, C], fb: F[B, C])(f: Tuple2K[A, B, *] ~>: X): F[X, C]
}
