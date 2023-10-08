package perspective

/** A higher kinded function from one kind to another. */
type FunctionK[A[_], B[_]] = [Z] => A[Z] => B[Z]
object FunctionK {

  /** The identity function. */
  def identity[F[_]]: F :~>: F = [Z] => (fz: F[Z]) => fz

  /** A function that always returns a constant value. */
  def const[F[_], A](a: A): F :~>#: A = [Z] => (fz: F[Z]) => a

  /**
    * Lift a function F[Any] => A where F is contravariant to a [[FunctionK]]
    * operating on the kind and the [[Const]] type.
    * @tparam F
    *   The contravariant kind
    * @tparam A
    *   The type to use as the const type.
    */
  def liftFromContravariant[F[+_], A](f: F[Any] => A): F :~>#: A = [Z] => (fa: F[Z]) => f(fa)

  /**
    * Lift a function A => F[Any] where F is contravariant to a [[FunctionK]]
    * operating on the kind and the [[Const]] type.
    *
    * @tparam F
    *   The contravariant kind
    * @tparam A
    *   The type to use as the const type.
    */
  def liftToContravariant[F[-_], A](f: A => F[Any]): A :#~>: F = [Z] => (a: A) => f(a)

  /**
    * Lift a function F[Nothing] => A where F is covariant to a [[FunctionK]]
    * operating on the kind and the [[Const]] type.
    *
    * @tparam F
    *   The covariant kind
    * @tparam A
    *   The type to use as the const type.
    */
  def liftFromCovariant[F[-_], A](f: F[Nothing] => A): F :~>#: A = [Z] => (fa: F[Z]) => f(fa)

  /**
    * Lift a function A => F[Nothing] where F is covariant to a [[FunctionK]]
    * operating on the kind and the [[Const]] type.
    *
    * @tparam F
    *   The covariant kind
    * @tparam A
    *   The type to use as the const type.
    */
  def liftToCovariant[F[+_], A](f: A => F[Nothing]): A :#~>: F = [Z] => (a: A) => f(a)

  /** Lift a normal function to a [[FunctionK]] operating on [[Const]] types. */
  def liftConst[A, B](f: A => B): A :#~>#: B = [Z] => (a: A) => f(a)
}

infix type :~>:[A[_], B[_]] = FunctionK[A, B]

/** A FunctionK returning a [[Const]] type. */
infix type :~>#:[F[_], R] = F :~>: Const[R]

/** A FunctionK taking a [[Const]] type. */
infix type :#~>:[T, F[_]] = Const[T] :~>: F

/** A FunctionK taking and returning [[Const]] types. */
infix type :#~>#:[T, R] = Const[T] :~>: Const[R]
