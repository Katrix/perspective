package perspective

trait ApplyK[F[_[_], _]] extends FunctorK[F]:
  extension[A[_], B[_], C](ff: F[[D] =>> A[D] => B[D], C]) def ap(fa: F[A, C]): F[B, C] =
    ff.map2K(fa)([Z] => (f: A[Z] => B[Z], a: A[Z]) => f(a))

  extension[A[_], B[_], Z[_], C](fa: F[A, C]) def map2K(fb: F[B, C])(f: [X] => (A[X], B[X]) => Z[X]): F[Z, C]

  extension[A[_], B[_], C](fa: F[A, C]) def tupledK(fb: F[B, C]): F[Tuple2K[A, B], C] = 
    fa.map2K(fb)([Z] => (a: A[Z], b: B[Z]) => (a, b))

type ApplyKC[F[_[_]]] = ApplyK[IgnoreC[F]]