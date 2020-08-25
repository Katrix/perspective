package perspective

import scala.deriving._
import scala.compiletime._
import scala.quoted._

trait FunctorK[F[_[_], _]]:
  type Curried[A[_]] = [C] =>> F[A, C]

  def [A[_], B[_], C](fa: F[A, C]) mapK (f: A ~>: B): F[B, C]

  def [A[_], B[_]](f: A ~>: B) liftK: Curried[A] ~>: Curried[B] = [C] => (fa: F[A, C]) => fa.mapK(f)

  def [A[_], C](fa: F[A, C]) voidK: F[Const[Unit], C] = fa.asK(ValueK.const(()))

  def [A[_], B[_], C](fa: F[A, C]) asK(b: ValueK[B]): F[B, C] = 
    fa.mapK([Z] => (_: A[Z]) => b[Z]())

  def [A[_], B[D] >: A[D], C](fa: F[A, C]) widen: F[B, C] = fa.asInstanceOf[F[B, C]]

type FunctorKC[F[_[_]]] = FunctorK[IgnoreC[F]]