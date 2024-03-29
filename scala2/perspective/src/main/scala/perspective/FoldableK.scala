package perspective

import cats.Monoid
import cats.instances.list._

trait FoldableK[F[_[_], _]] {

  def foldLeftK[A[_], B, C](fa: F[A, C], b: B)(f: B => A ~>#: B): B

  def foldMapK[A[_], B, C](fa: F[A, C])(f: A ~>#: B)(implicit B: Monoid[B]): B =
    foldLeftK(fa, B.empty)(b => λ[A ~>: Const[B, *]](a => B.combine(b, f(a))))

  def toListK[A, C](fa: F[Const[A, *], C]): List[A] =
    foldMapK(fa)(FunctionK.liftConst(List(_)))
}
