package perspective

type FunctionK[A[_], B[_]] = [Z] => A[Z] => B[Z]
object FunctionK {
  def identity[F[_]]: F ~>: F = [Z] => (fz: F[Z]) => fz

  def const[F[_], A](a: A): F ~>#: A = [Z] => (fz: F[Z]) => a

  def liftFromContravariant[F[+_], A](f: F[Any] => A): F ~>#: A = [Z] => (fa: F[Z]) => f(fa)
  def liftToContravariant[F[-_], A](f: A => F[Any]): A #~>: F   = [Z] => (a: A) => f(a)
  def liftFromCovariant[F[-_], A](f: F[Nothing] => A): F ~>#: A = [Z] => (fa: F[Z]) => f(fa)
  def liftToCovariant[F[+_], A](f: A => F[Nothing]): A #~>: F   = [Z] => (a: A) => f(a)

  def liftConst[A, B](f: A => B): A #~>#: B = [Z] => (a: A) => f(a)
}

type ~>:[A[_], B[_]] = FunctionK[A, B]

type ~>#:[F[_], R] = F ~>: Const[R]
type #~>:[T, F[_]] = Const[T] ~>: F
type #~>#:[T, R] = Const[T] ~>: Const[R]