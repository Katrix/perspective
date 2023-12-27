package perspective
package instances

import cats.Applicative

given idInstanceC[T]: (TraverseKC[IdFC[T]] & RepresentableKC[IdFC[T]] {type RepresentationK[_] = Finite[1]}) =
  new TraverseKC[IdFC[T]] with RepresentableKC[IdFC[T]] {
    override type RepresentationK[_] = Finite[1]

    extension [A[_], C](fa: A[T])
      override def indexK[Z](i: Finite[1]): A[Z] = fa.asInstanceOf[A[Z]]
      override def foldLeftK[B](b: B)(f: B => A :~>#: B): B = f(b)(fa)
      override def foldRightK[B](b: B)(f: A :~>#: (B => B)): B = f(fa)(b)
      override def traverseK[G[_] : Applicative, B[_]](f: A :~>: Compose2[G, B]): G[B[T]] = f(fa)

    override def tabulateK[A[_], C](f: Finite[1] :#~>: A): A[T] = f(Finite(1, 1))
  }
