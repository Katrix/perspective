package perspective

/** A value that can be be applied to any type. */
type ValueK[A[_]] = [Z] => () => A[Z]
object ValueK {

  /** Construct a [[ValueK]] from a covariant higher kinded type. */
  def co[A[+_]](covariant: A[Nothing]): ValueK[A] = [Z] => () => covariant

  /** Construct a [[ValueK]] from a contravariant higher kinded type. */
  def contra[A[-_]](contravariant: A[Any]): ValueK[A] = [Z] => () => contravariant

  /** Construct a [[ValueK]] of a constant. */
  def const[A](a: A): ValueK[Const[A]] = [Z] => () => a
}
