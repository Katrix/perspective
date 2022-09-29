package perspective

/** A higher kinded type that is always the type given no matter what. */
type Const[A] = [_] =>> A

/**
  * A function that adapts typeclasses expecting a type having both an higher
  * kinded type and a non higher kinded type.
  */
type IgnoreC[F[_[_]]] = [A[_], _] =>> F[A]
