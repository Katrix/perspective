package perspective

import scala.deriving._
import scala.compiletime._
import scala.quoted._

trait FunctorK[F[_[_], _]]:
  extension[A[_], C](fa: F[A, C]) 
    def mapK[B[_]](f: A ~>: B): F[B, C]

    inline def mapConst[B](f: A ~>#: B): F[Const[B], C] =
      mapK(f)

    inline def voidK: F[Const[Unit], C] = asK(ValueK.const(()))

    inline def asK[B[_]](b: ValueK[B]): F[B, C] =
      mapK([Z] => (_: A[Z]) => b[Z]())

    inline def widen[B[D] >: A[D]]: F[B, C] = fa.asInstanceOf[F[B, C]]
  
  extension [A[_], B[_]](f: A ~>: B) def liftK: F[A, *] ~>: F[B, *] = [C] => (fa: F[A, C]) => fa.mapK(f)

type FunctorKC[F[_[_]]] = FunctorK[IgnoreC[F]]