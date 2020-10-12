package perspective

trait ApplicativeK[F[_[_], _]] extends ApplyK[F]:
  extension [A[_], C](a: ValueK[A]) def pure: F[A, C]

  def unitK[C]: F[Const[Unit], C] = ValueK.const(()).pure

  extension[A[_], B[_], C](fa: F[A, C]) override def mapK(f: A ~>: B): F[B, C] =
    ([Z] => () => f[Z]).pure[[D] =>> A[D] => B[D], C].ap(fa)

type ApplicativeKC[F[_[_]]] = ApplicativeK[IgnoreC[F]]