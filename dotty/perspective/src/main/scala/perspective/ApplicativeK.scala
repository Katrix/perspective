package perspective

/** A higher kinded [[cats.Applicative]] typeclass. */
trait ApplicativeK[F[_[_], _]] extends ApplyK[F]:
  /** A higher kinded equivalent of [[cats.Applicative.pure]]. */
  extension [A[_]](a: ValueK[A]) def pure[C]: F[A, C]

  /** A higher kinded equivalent of [[cats.Applicative.unit]]. */
  def unitK[C]: F[Const[Unit], C] = ValueK.const(()).pure

  extension [A[_], C](fa: F[A, C])
    override def mapK[B[_]](f: A :~>: B): F[B, C] =
      this.pure[[Z] =>> A[Z] => B[Z]]([Z] => () => f[Z])[C].ap(fa)
object ApplicativeK:
  given idInstanceC[A]: ApplicativeKC[IdFC[A]] = instances.idInstanceC[A]

/**
  * A version of [[ApplicativeK]] without a normal type as well as a higher
  * kinded type.
  */
type ApplicativeKC[F[_[_]]] = ApplicativeK[IgnoreC[F]]
