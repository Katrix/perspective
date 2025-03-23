package perspective

import cats.Functor
import cats.syntax.all.*

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

/** A higher kinded [[cats.Functor]] typeclass. */
trait FunctorK[F[_[_], _]] extends InvariantK[F]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Functor.map]]. */
    def mapK[B[_]](f: A :~>: B): F[B, C]

    override def imapK[B[_]](f: A :~>: B)(g: B :~>: A): F[B, C] = mapK(f)

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

object FunctorK:
  given idInstanceC[A]: FunctorKC[IdFC[A]] = instances.idInstanceC[A]

  given composeCatsOutside[F[_], G[_[_]]](using F: Functor[F], G: FunctorKC[G]): FunctorKC[[H[_]] =>> F[G[H]]] with
    extension [A[_], C](fa: F[G[A]])
      override def mapK[B[_]](f: A :~>: B): F[G[B]] = fa.map(_.mapK(f))

  given composeId[F[_], X](using F: Functor[F]): FunctorKC[[H[_]] =>> F[H[X]]] = composeCatsOutside[F, IdFC[X]]

  given composeCatsInside[F[_[_]], G[_]](using F: FunctorKC[F], G: Functor[G]): FunctorKC[[H[_]] =>> F[Compose2[G, H]]] with {
    extension [A[_], C](fga: F[Compose2[G, A]])
      override def mapK[B[_]](f: A :~>: B): F[Compose2[G, B]] = F.mapK(fga)([X] => (ga: G[A[X]]) => ga.map(a => f(a)))
  }

  given composeCatsInsideRight[F[_[_]], G[_]](using F: FunctorKC[F]): FunctorKC[[H[_]] =>> F[Compose2[H, G]]] with {
    extension [A[_], C](fag: F[Compose2[A, G]])
      override def mapK[B[_]](f: A :~>: B): F[Compose2[B, G]] = F.mapK(fag)([X] => (ag: A[G[X]]) => f(ag))
  }

/**
  * A version of [[FunctorK]] without a normal type as well as a higher kinded
  * type.
  */
type FunctorKC[F[_[_]]] = FunctorK[IgnoreC[F]]
