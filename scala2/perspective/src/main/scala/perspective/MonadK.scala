package perspective

//TODO: Check if this is the valid way to define MonadK
trait MonadK[F[_[_], _]] extends ApplicativeK[F] {

  def flattenK[A[_], C](ffa: F[F[A, *], C]): F[A, C] = flatMapK(ffa)(FunctionK.identity)

  def flatMapK[A[_], B[_], C](fa: F[A, C])(f: A ~>: F[B, *]): F[B, C]

  //TODO: Implement mapK and map2K in terms of flatMapK
}
