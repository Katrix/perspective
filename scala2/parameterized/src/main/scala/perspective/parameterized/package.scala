package perspective

import cats.{Applicative, Apply, Functor}

package object parameterized extends LowPriorityPackage1 {

  type Const[A, B] = Const.Const[A, B]

  type IgnoreC[F[_[_]]] = {
    type λ[A[_], _] = F[A]
  }

  type Compose2[A[_], B[_], C] = cats.data.Nested[A, B, C]
  val Compose2: cats.data.Nested.type = cats.data.Nested

  type ~>:[F[_], G[_]] = FunctionK[F, G]

  type Tuple2K[F[_], G[_], A] = cats.data.Tuple2K[F, G, A]
  val Tuple2K: cats.data.Tuple2K.type = cats.data.Tuple2K

  type IdFC[A] = { type λ[F0[_]]    = F0[A] }
  type IdF     = { type λ[F0[_], A] = F0[A] }

  // format: off
  type Tuple1F[A] = {type λ[F0[_]] = Tuple1[F0[A]]}
  type Tuple2F[A, B] = {type λ[F0[_]] = Tuple2[F0[A], F0[B]]}
  type Tuple3F[A, B, C] = {type λ[F0[_]] = Tuple3[F0[A], F0[B], F0[C]]}
  type Tuple4F[A, B, C, D] = {type λ[F0[_]] = Tuple4[F0[A], F0[B], F0[C], F0[D]]}
  type Tuple5F[A, B, C, D, E] = {type λ[F0[_]] = Tuple5[F0[A], F0[B], F0[C], F0[D], F0[E]]}
  type Tuple6F[A, B, C, D, E, F] = {type λ[F0[_]] = Tuple6[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F]]}
  type Tuple7F[A, B, C, D, E, F, G] = {type λ[F0[_]] = Tuple7[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G]]}
  type Tuple8F[A, B, C, D, E, F, G, H] = {type λ[F0[_]] = Tuple8[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H]]}
  type Tuple9F[A, B, C, D, E, F, G, H, I] = {type λ[F0[_]] = Tuple9[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I]]}
  type Tuple10F[A, B, C, D, E, F, G, H, I, J] = {type λ[F0[_]] = Tuple10[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J]]}
  type Tuple11F[A, B, C, D, E, F, G, H, I, J, K] = {type λ[F0[_]] = Tuple11[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K]]}
  type Tuple12F[A, B, C, D, E, F, G, H, I, J, K, L0] = {type λ[F0[_]] = Tuple12[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0]]}
  type Tuple13F[A, B, C, D, E, F, G, H, I, J, K, L0, M] = {type λ[F0[_]] = Tuple13[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M]]}
  type Tuple14F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N] = {type λ[F0[_]] = Tuple14[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N]]}
  type Tuple15F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N, O] = {type λ[F0[_]] = Tuple15[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N], F0[O]]}
  type Tuple16F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N, O, P] = {type λ[F0[_]] = Tuple16[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N], F0[O], F0[P]]}
  type Tuple17F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N, O, P, Q] = {type λ[F0[_]] = Tuple17[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N], F0[O], F0[P], F0[Q]]}
  type Tuple18F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N, O, P, Q, R] = {type λ[F0[_]] = Tuple18[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N], F0[O], F0[P], F0[Q], F0[R]]}
  type Tuple19F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N, O, P, Q, R, S] = {type λ[F0[_]] = Tuple19[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N], F0[O], F0[P], F0[Q], F0[R], F0[S]]}
  type Tuple20F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N, O, P, Q, R, S, T] = {type λ[F0[_]] = Tuple20[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N], F0[O], F0[P], F0[Q], F0[R], F0[S], F0[T]]}
  type Tuple21F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N, O, P, Q, R, S, T, U] = {type λ[F0[_]] = Tuple21[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N], F0[O], F0[P], F0[Q], F0[R], F0[S], F0[T], F0[U]]}
  type Tuple22F[A, B, C, D, E, F, G, H, I, J, K, L0, M, N, O, P, Q, R, S, T, U, V] = {type λ[F0[_]] = Tuple22[F0[A], F0[B], F0[C], F0[D], F0[E], F0[F], F0[G], F0[H], F0[I], F0[J], F0[K], F0[L0], F0[M], F0[N], F0[O], F0[P], F0[Q], F0[R], F0[S], F0[T], F0[U], F0[V]]}
  // format: on

  type PFunctorKC[F[_[_]]]     = PFunctorK[IgnoreC[F]#λ]
  type PApplyKC[F[_[_]]]       = PApplyK[IgnoreC[F]#λ]
  type PApplicativeKC[F[_[_]]] = PApplicativeK[IgnoreC[F]#λ]
  type PMonadKC[F[_[_]]]       = PMonadK[IgnoreC[F]#λ]

  type INothing <: Nothing

  implicit val idInstance: PApplicativeK[IdF#λ] = new PApplicativeK[IdF#λ] {
    override def pureK[A[_]: Applicative, C](a: Const[Unit, *] ~>: A): A[C] = a(Const.unit)

    override def map2K[A[_]: Apply, B[_]: Apply, Z[_]: Apply, C](fa: A[C], fb: B[C])(f: Tuple2K[A, B, *] ~>: Z): Z[C] =
      f(Tuple2K(fa, fb))

    override def apK[A[_]: Apply, B[_]: Apply, C](ff: A[C] => B[C])(fa: A[C]): B[C] = ff(fa)

    override def mapK[A[_]: Functor, B[_]: Functor, C](fa: A[C])(f: A ~>: B): B[C] = f(fa)
  }
}

package parameterized {
  import cats.Functor

  trait LowPriorityPackage1 {

    implicit def idInstanceC[T]: PApplicativeKC[IdFC[T]#λ] = new PApplicativeKC[IdFC[T]#λ] {
      override def pureK[A[_]: Applicative, C](a: Const[Unit, *] ~>: A): A[T] = a(Const.unit)

      override def map2K[A[_]: Apply, B[_]: Apply, Z[_]: Apply, C](fa: A[T], fb: B[T])(
          f: Tuple2K[A, B, *] ~>: Z
      ): Z[T] = f(Tuple2K(fa, fb))

      override def apK[A[_]: Apply, B[_]: Apply, C](ff: A[T] => B[T])(fa: A[T]): B[T] = ff(fa)

      override def mapK[A[_]: Functor, B[_]: Functor, C](fa: A[T])(f: A ~>: B): B[T] = f(fa)
    }
  }
}
