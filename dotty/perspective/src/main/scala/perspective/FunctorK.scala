package perspective

import scala.deriving._
import scala.compiletime._
import scala.quoted._

trait FunctorK[F[_[_], _]]:
  extension[A[_], B[_], C](fa: F[A, C]) def mapK (f: A ~>: B): F[B, C]
  
  extension[A[_], B, C](fa: F[A, C]) def mapConst (f: A ~>#: B): F[Const[B], C] =
    fa.mapK(f)

  extension [A[_], B[_]](f: A ~>: B) def liftK: F[A, *] ~>: F[B, *] = [C] => (fa: F[A, C]) => fa.mapK(f)

  extension [A[_], C](fa: F[A, C]) def voidK: F[Const[Unit], C] = fa.asK(ValueK.const(()))

  extension [A[_], B[_], C](fa: F[A, C]) def asK(b: ValueK[B]): F[B, C] = 
    fa.mapK([Z] => (_: A[Z]) => b[Z]())

  extension [A[_], B[D] >: A[D], C](fa: F[A, C]) def widen: F[B, C] = fa.asInstanceOf[F[B, C]]

type FunctorKC[F[_[_]]] = FunctorK[IgnoreC[F]]