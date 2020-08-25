package perspective

trait ApplicativeK[F[_[_], _]] extends ApplyK[F]:
  def [A[_], C](a: ValueK[A]) pure: F[A, C]

  def unitK[C]: F[Const[Unit], C] = ValueK.const(()).pure

  override def [A[_], B[_], C](fa: F[A, C]) mapK(f: A ~>: B): F[B, C] =
    ([Z] => () => f[Z]).pure[[D] =>> A[D] => B[D], C].ap(fa)

type ApplicativeKC[F[_[_]]] = ApplicativeK[IgnoreC[F]]