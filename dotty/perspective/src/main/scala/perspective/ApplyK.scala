package perspective

trait ApplyK[F[_[_], _]] extends FunctorK[F]:
  def [A[_], B[_], C](ff: F[[D] =>> A[D] => B[D], C]) ap(fa: F[A, C]): F[B, C] =
    ff.map2K(fa)([Z] => (t: Tuple2K[[D] =>> A[D] => B[D], A][Z]) => t._1(t._2))

  def [A[_], B[_], Z[_], C](fa: F[A, C]) map2K(fb: F[B, C])(f: Tuple2K[A, B] ~>: Z): F[Z, C]

  def [A[_], B[_], C](fa: F[A, C]) tupledK(fb: F[B, C]): F[Tuple2K[A, B], C] = 
    fa.map2K(fb)(FunctionK.identity[Tuple2K[A, B]])

type ApplyKC[F[_[_]]] = ApplyK[IgnoreC[F]]