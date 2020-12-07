import cats.{Applicative, Functor}

package object perspective extends LowPriorityPackage1 {

  type Const[A, B] = A

  type IgnoreC[F[_[_]]] = {
    type λ[A[_], _] = F[A]
  }

  type Compose2[A[_], B[_], C]                   = A[B[C]]
  type Compose3[A[_], B[_], C[_], Z]             = A[B[C[Z]]]
  type Compose4[A[_], B[_], C[_], D[_], Z]       = A[B[C[D[Z]]]]
  type Compose5[A[_], B[_], C[_], D[_], E[_], Z] = A[B[C[D[E[Z]]]]]

  type ~>:[-F[_], +G[_]] = FunctionK[F, G]

  type ~>#:[F[_], R] = F ~>: Const[R, *]
  type #~>:[T, F[_]] = Const[T, *] ~>: F
  type #~>#:[T, R]   = Const[T, *] ~>: Const[R, *]

  type Tuple2K[F[_], G[_], A]                   = (F[A], G[A])
  type Tuple3K[F[_], G[_], H[_], A]             = (F[A], G[A], H[A])
  type Tuple4K[F[_], G[_], H[_], I[_], A]       = (F[A], G[A], H[A], I[A])
  type Tuple5K[F[_], G[_], H[_], I[_], J[_], A] = (F[A], G[A], H[A], I[A], J[A])

  // format: off
  type Tuple2KK[F[_[_], _], G[_[_], _]]                                     = { type λ[A[_], C] = (F[A, C], G[A, C]) }
  type Tuple3KK[F[_[_], _], G[_[_], _], H[_[_], _]]                         = { type λ[A[_], C] = (F[A, C], G[A, C], H[A, C]) }
  type Tuple4KK[F[_[_], _], G[_[_], _], H[_[_], _], I[_[_], _]]             = {type λ[A[_], C] = (F[A, C], G[A, C], H[A, C], I[A, C])}
  type Tuple5KK[F[_[_], _], G[_[_], _], H[_[_], _], I[_[_], _], J[_[_], _]] = {type λ[A[_], C] = (F[A, C], G[A, C], H[A, C], I[A, C], J[A, C])}
  // format: on

  type IdFC[A] = { type λ[F0[_]] = F0[A] }
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

  type FunctorKC[F[_[_]]]       = FunctorK[IgnoreC[F]#λ]
  type ApplyKC[F[_[_]]]         = ApplyK[IgnoreC[F]#λ]
  type ApplicativeKC[F[_[_]]]   = ApplicativeK[IgnoreC[F]#λ]
  type MonadKC[F[_[_]]]         = MonadK[IgnoreC[F]#λ]
  type RepresentableKC[F[_[_]]] = RepresentableK[IgnoreC[F]#λ]
  object RepresentableKC {
    type Aux[F[_[_]], RepresentationK0[_]] = RepresentableK[IgnoreC[F]#λ] {
      type RepresentationK[A] = RepresentationK0[A]
    }
  }
  type FoldableKC[F[_[_]]]     = FoldableK[IgnoreC[F]#λ]
  type TraverseKC[F[_[_]]]     = TraverseK[IgnoreC[F]#λ]
  type DistributiveKC[F[_[_]]] = DistributiveK[IgnoreC[F]#λ]

  type INothing <: Nothing

  implicit val idInstance: TraverseK[IdF#λ] with RepresentableK[IdF#λ] { type RepresentationK[_] = Finite[1] } =
    new TraverseK[IdF#λ] with RepresentableK[IdF#λ] {
      override def traverseK[G[_]: Applicative, A[_], B[_], C](fa: A[C])(f: A ~>: Compose2[G, B, *]): G[B[C]] = f(fa)

      override def foldLeftK[A[_], B, C](fa: A[C], b: B)(f: B => A ~>#: B): B = f(b)(fa)

      override type RepresentationK[_] = Finite[1]

      override def indexK[A[_], C](fa: A[C]): Finite[1] #~>: A = new (Finite[1] #~>: A) {
        override def apply[Z](i: Finite[1]): A[Z] = fa.asInstanceOf[A[Z]]
      }

      override def tabulateK[A[_], C](f: Finite[1] #~>: A): A[C] = f(Finite(1, 1))
    }
}

package perspective {
  trait LowPriorityPackage1 {

    implicit def idInstanceC[T]: TraverseKC[IdFC[T]#λ] with RepresentableKC[IdFC[T]#λ] { type RepresentationK[_] = Finite[1] } =
      new TraverseKC[IdFC[T]#λ] with RepresentableKC[IdFC[T]#λ] {
        override def traverseK[G[_]: Applicative, A[_], B[_], C](fa: A[T])(f: A ~>: Compose2[G, B, *]): G[B[T]] = f(fa)

        override def foldLeftK[A[_], B, C](fa: A[T], b: B)(f: B => A ~>#: B): B = f(b)(fa)

        override type RepresentationK[_] = Finite[1]

        override def indexK[A[_], C](fa: A[T]): Finite[1] #~>: A = new (Finite[1] #~>: A) {
          override def apply[Z](i: Finite[1]): A[Z] = fa.asInstanceOf[A[Z]]
        }

        override def tabulateK[A[_], C](f: Finite[1] #~>: A): A[T] = f(Finite(1, 1))
      }
  }
}
