package perspective

import cats.Applicative

/** An identity type constructor. */
type Id[A] = A

type IdFC[A] = [F0[_]] =>> F0[A]
given idInstanceC[T]: (TraverseKC[IdFC[T]] & RepresentableKC[IdFC[T]] { type RepresentationK[_] = Finite[1] }) =
  new TraverseKC[IdFC[T]] with RepresentableKC[IdFC[T]] {
    override type RepresentationK[_] = Finite[1]

    extension [A[_], C](fa: A[T]) override def indexK[Z](i: Finite[1]): A[Z] = fa.asInstanceOf[A[Z]]
    extension [A[_], C](fa: A[T])
      override def foldLeftK[B](b: B)(f: B => A :~>#: B): B    = f(b)(fa)
      override def foldRightK[B](b: B)(f: A :~>#: (B => B)): B = f(fa)(b)
      override def traverseK[G[_]: Applicative, B[_]](f: A :~>: Compose2[G, B]): G[B[T]] = f(fa)

    override def tabulateK[A[_], C](f: Finite[1] :#~>: A): A[T] = f(Finite(1, 1))
  }
