package perspective.parameterized

import cats.Applicative
import cats.kernel.Monoid
import simulacrum.typeclass

@typeclass trait PApplicativeK[F[_[_], _]] extends PApplyK[F] {

  def pureK[A[_]: Applicative, C](a: Const[Unit, *] ~>: A): F[A, C]

  def unitK[C]: F[Const[Unit, *], C] =
    pureK(FunctionK.identity)(Const.constApplicativeInstance(new Monoid[Unit] {
      override def empty: Unit = ()

      override def combine(x: Unit, y: Unit): Unit = ()
    }))
}
