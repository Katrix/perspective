package perspective

type FunctionK[A[_], B[_]] = [Z] => A[Z] => B[Z]
object FunctionK {
  def identity[F[_]]: F ~>: F = [Z] => (fz: F[Z]) => fz

  def const[F[_], A](a: A): F ~>#: A = [Z] => (fz: F[Z]) => a
}

type ~>:[A[_], B[_]] = FunctionK[A, B]

type ~>#:[F[_], R] = F ~>: Const[R]
type #~>:[T, F[_]] = Const[T] ~>: F
type #~>#:[T, R] = Const[T] ~>: Const[R]