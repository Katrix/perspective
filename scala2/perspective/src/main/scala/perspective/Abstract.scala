package perspective

trait Abstract[A] {
  type F[_]
  type Out[G[_], C]
  type C

  def convert(a: A): Out[F, C]
}
object Abstract {
  type Aux[F0[_], A, Out0[_[_], _], C0] = Abstract[A] {
    type F[B] = F0[B]; type Out[G[_], C] = Out0[G, C]; type C = C0
  }

  def apply[A](implicit abs: Abstract[A]): Aux[abs.F, A, abs.Out, abs.C] = abs

  implicit def valueAbstract[F0[_], A]: Aux[F0, F0[A], λ[(G[_], D) => G[A]], INothing] = new Abstract[F0[A]] {
    type F[B]         = F0[B]
    type Out[G[_], _] = G[A]
    type C            = INothing

    override def convert(a: F0[A]): F0[A] = a
  }

  implicit def hkdAbstract[F0[_], A[_[_]]]: Aux[F0, A[F0], λ[(G[_], D) => A[G]], INothing] = new Abstract[A[F0]] {
    type F[B]         = F0[B]
    type Out[G[_], _] = A[G]
    type C            = INothing

    override def convert(a: A[F0]): A[F0] = a
  }

  implicit def hkdAbstractWithC[F0[_], A[_[_], _], C0]: Aux[F0, A[F0, C0], λ[(G[_], D) => A[G, D]], C0] =
    new Abstract[A[F0, C0]] {
      type F[B]         = F0[B]
      type Out[G[_], D] = A[G, D]
      type C            = C0

      override def convert(a: A[F0, C0]): A[F0, C0] = a
    }
}
