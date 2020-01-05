package perspective

type ValueK[A[_]] = [Z] => () => A[Z]
object ValueK {

  def co[A[+_]](an: A[Nothing]): ValueK[A] = [Z] => () => an
  def contra[A[-_]](aa: A[Any]): ValueK[A] = [Z] => () => aa

  def const[A](a: A): ValueK[Const[A]] = [Z] => () => a
}