package perspective

//TODO: Check if this is the valid way to define MonadK
trait MonadK[F[_[_], _]] extends ApplicativeK[F]:

  extension [A[_], C](ffa: F[F[A, *], C]) def flattenK: F[A, C] = ffa.flatMapK(FunctionK.identity[F[A, *]])

  extension [A[_], C](fa: F[A, C]) def flatMapK[B[_]](f: A :~>: F[B, *]): F[B, C]

  // TODO: Implement mapK and map2K in terms of flatMapK
object MonadK:
    given idInstanceC[A]: MonadKC[IdFC[A]] = instances.idInstanceC[A]

type MonadKC[F[_[_]]] = MonadK[IgnoreC[F]]
