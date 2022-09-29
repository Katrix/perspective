package perspective

/** The composition of 2 higher kinded types. */
type Compose2[A[_], B[_]] = [Z] =>> A[B[Z]]

/** The composition of 3 higher kinded types. */
type Compose3[A[_], B[_], C[_]] = [Z] =>> A[B[C[Z]]]

/** The composition of 4 higher kinded types. */
type Compose4[A[_], B[_], C[_], D[_]] = [Z] =>> A[B[C[D[Z]]]]

/** The composition of 5 higher kinded types. */
type Compose5[A[_], B[_], C[_], D[_], E[_]] = [Z] =>> A[B[C[D[E[Z]]]]]
