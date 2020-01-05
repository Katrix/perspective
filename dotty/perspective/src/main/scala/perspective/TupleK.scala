package perspective

type Tuple2K[A[_], B[_]]                   = [Z] =>> (A[Z], B[Z])
type Tuple3K[A[_], B[_], C[_]]             = [Z] =>> (A[Z], B[Z], C[Z])
type Tuple4K[A[_], B[_], C[_], D[_]]       = [Z] =>> (A[Z], B[Z], C[Z], D[Z])
type Tuple5K[A[_], B[_], C[_], D[_], E[_]] = [Z] =>> (A[Z], B[Z], C[Z], D[Z], E[Z])

type Tuple2KK[A[_[_], _], B[_[_], _]]                                     = [Z[_], ZC] =>> (A[Z, ZC], B[Z, ZC])
type Tuple3KK[A[_[_], _], B[_[_], _], C[_[_], _]]                         = [Z[_], ZC] =>> (A[Z, ZC], B[Z, ZC], C[Z, ZC])
type Tuple4KK[A[_[_], _], B[_[_], _], C[_[_], _], D[_[_], _]]             = [Z[_], ZC] =>> (A[Z, ZC], B[Z, ZC], C[Z, ZC], D[Z, ZC])
type Tuple5KK[A[_[_], _], B[_[_], _], C[_[_], _], D[_[_], _], E[_[_], _]] = [Z[_], ZC] =>> (A[Z, ZC], B[Z, ZC], C[Z, ZC], D[Z, ZC], E[Z, ZC])