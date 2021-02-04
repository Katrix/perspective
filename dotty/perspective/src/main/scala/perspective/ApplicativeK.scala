package perspective

trait ApplicativeK[F[_[_], _]] extends ApplyK[F]:
  extension [A[_]](a: ValueK[A]) def pure[C]: F[A, C]

  def unitK[C]: F[Const[Unit], C] = ValueK.const(()).pure

  extension[A[_], C](fa: F[A, C]) override def mapK[B[_]](f: A ~>: B): F[B, C] = 
    this.pure[[Z] =>> A[Z] => B[Z]]([Z] => () => f[Z])[C].ap(fa)

type ApplicativeKC[F[_[_]]] = ApplicativeK[IgnoreC[F]]