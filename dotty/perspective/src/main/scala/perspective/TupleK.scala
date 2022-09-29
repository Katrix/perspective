package perspective

/** A higher kinded Tuple2. */
type Tuple2K[A[_], B[_]] = [Z] =>> (A[Z], B[Z])

/** A higher kinded Tuple3. */
type Tuple3K[A[_], B[_], C[_]] = [Z] =>> (A[Z], B[Z], C[Z])

/** A higher kinded Tuple4. */
type Tuple4K[A[_], B[_], C[_], D[_]] = [Z] =>> (A[Z], B[Z], C[Z], D[Z])

/** A higher kinded Tuple5. */
type Tuple5K[A[_], B[_], C[_], D[_], E[_]] = [Z] =>> (A[Z], B[Z], C[Z], D[Z], E[Z])

/** An even higher kinded Tuple2. */
type Tuple2KK[A[_[_], _], B[_[_], _]] = [Z[_], ZC] =>> (A[Z, ZC], B[Z, ZC])

/** An even higher kinded Tuple3. */
type Tuple3KK[A[_[_], _], B[_[_], _], C[_[_], _]] = [Z[_], ZC] =>> (A[Z, ZC], B[Z, ZC], C[Z, ZC])

/** An even higher kinded Tuple4. */
type Tuple4KK[A[_[_], _], B[_[_], _], C[_[_], _], D[_[_], _]] = [Z[_], ZC] =>> (A[Z, ZC], B[Z, ZC], C[Z, ZC], D[Z, ZC])

/** An even higher kinded Tuple5. */
type Tuple5KK[A[_[_], _], B[_[_], _], C[_[_], _], D[_[_], _], E[_[_], _]] =
  [Z[_], ZC] =>> (A[Z, ZC], B[Z, ZC], C[Z, ZC], D[Z, ZC], E[Z, ZC])
