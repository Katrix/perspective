package perspective

import cats._
import cats.syntax.all._

import scala.language.implicitConversions

trait FoldableK[F[_[_], _]]:
  extension[A[_], C](fa: F[A, C]) 
    def foldLeftK[B](b: B)(f: B => A ~>#: B): B
    
    def foldMapK[B](f: A ~>#: B)(using B: Monoid[B]): B =
      foldLeftK(B.empty)(b => [Z] => (az: A[Z]) => b.combine(f(az)))

  extension[A, C](fa: F[Const[A], C]) def toListK: List[A] =
    fa.foldMapK(FunctionK.liftConst(List(_: A)))

type FoldableKC[F[_[_]]] = FoldableK[IgnoreC[F]]