package perspective

type Const[A] = [_] =>> A
type IgnoreC[F[_[_]]] = [A[_], _] =>> F[A]