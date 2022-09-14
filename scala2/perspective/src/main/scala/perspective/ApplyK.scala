package perspective

trait ApplyK[F[_[_], _]] extends FunctorK[F] {

  def apK[A[_], B[_], C](ff: F[λ[D => A[D] => B[D]], C])(fa: F[A, C]): F[B, C] =
    map2K(ff, fa)(λ[Tuple2K[λ[D => A[D] => B[D]], A, *] ~>: B](t => t._1(t._2)))

  def tupledK[A[_], B[_], C](fa: F[A, C], fb: F[B, C]): F[Tuple2K[A, B, *], C] =
    map2K(fa, fb)(FunctionK.identity)

  def map2K[A[_], B[_], Z[_], C](fa: F[A, C], fb: F[B, C])(f: Tuple2K[A, B, *] ~>: Z): F[Z, C]
}
