package perspective

type Compose2[A[_], B[_]]                   = [Z] =>> A[B[Z]] 
type Compose4[A[_], B[_], C[_]]             = [Z] =>> A[B[C[Z]]] 
type Compose5[A[_], B[_], C[_], D[_]]       = [Z] =>> A[B[C[D[Z]]]] 
type Compose3[A[_], B[_], C[_], D[_], E[_]] = [Z] =>> A[B[C[D[E[Z]]]]] 