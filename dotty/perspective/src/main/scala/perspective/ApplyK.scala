package perspective

/** A higher kinded [[cats.Apply]] typeclass. */
trait ApplyK[F[_[_], _]] extends FunctorK[F]:
  /** A higher kinded equivalent of [[cats.Apply.ap]]. */
  extension [A[_], B[_], C](ff: F[[D] =>> A[D] => B[D], C])
    def ap(fa: F[A, C]): F[B, C] =
      ff.map2K(fa)([Z] => (f: A[Z] => B[Z], a: A[Z]) => f(a))

  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Apply.map2]]. */
    def map2K[B[_], Z[_]](fb: F[B, C])(f: [X] => (A[X], B[X]) => Z[X]): F[Z, C]

    /** Helper function that calls [[map2K]] with [[Const]]. */
    inline def map2Const[B[_], Z](fb: F[B, C])(f: [X] => (A[X], B[X]) => Z): F[Const[Z], C] =
      fa.map2K(fb)(f)

    /** A higher kinded equivalent of [[cats.Apply.product]]. */
    def tupledK[B[_]](fb: F[B, C]): F[Tuple2K[A, B], C] =
      fa.map2K(fb)([Z] => (a: A[Z], b: B[Z]) => (a, b))

object ApplyK:
  given idInstanceC[A]: ApplyKC[IdFC[A]] = instances.idInstanceC[A]

/**
  * A version of [[ApplyK]] without a normal type as well as a higher kinded
  * type.
  */
type ApplyKC[F[_[_]]] = ApplyK[IgnoreC[F]]
