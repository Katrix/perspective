package perspective

import scala.compiletime._
import scala.deriving._
import scala.quoted._

/** A higher kinded [[cats.Functor]] typeclass. */
trait FunctorK[F[_[_], _]]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Functor.map]]. */
    def mapK[B[_]](f: A :~>: B): F[B, C]

    /** Helper function that calls [[mapK]] with [[Const]]. */
    inline def mapConst[B](f: A :~>#: B): F[Const[B], C] =
      mapK(f)

    /** A higher kinded equivalent of [[cats.Functor.void]]. */
    inline def voidK: F[Const[Unit], C] = asK(ValueK.const(()))

    /** A higher kinded equivalent of [[cats.Functor.as]]. */
    inline def asK[B[_]](b: ValueK[B]): F[B, C] =
      mapK([Z] => (_: A[Z]) => b[Z]())

    /** A higher kinded equivalent of [[cats.Functor.widen]]. */
    inline def widen[B[D] >: A[D]]: F[B, C] = fa.asInstanceOf[F[B, C]]

  /** A higher kinded equivalent of [[cats.Functor.lift]]. */
  extension [A[_], B[_]](f: A :~>: B) def liftK: F[A, *] :~>: F[B, *] = [C] => (fa: F[A, C]) => fa.mapK(f)

/**
  * A version of [[FunctorK]] without a normal type as well as a higher kinded
  * type.
  */
type FunctorKC[F[_[_]]] = FunctorK[IgnoreC[F]]
