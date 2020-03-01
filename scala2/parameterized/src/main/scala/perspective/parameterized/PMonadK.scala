package perspective.parameterized

import cats.Monad
import simulacrum.typeclass

//TODO: Check if this is the valid way to define MonadK
@typeclass trait PMonadK[F[_[_], _]] extends PApplicativeK[F] {

  def flattenK[A[_]: Monad, C](ffa: F[F[A, *], C]): F[A, C]

  def flatMapK[A[_]: Monad, B[_]: Monad, C](fa: F[A, C])(f: A ~>: F[B, *]): F[B, C]
}
