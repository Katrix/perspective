package perspective

import cats.Invariant
import cats.syntax.all.*

trait InvariantK[F[_[_], _]]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Invariant.imap]]. */
    def imapK[B[_]](f: A :~>: B)(g: B :~>: A): F[B, C]

object InvariantK:
  given idInstanceC[A]: InvariantKC[IdFC[A]] = instances.idInstanceC[A]

  given composeCats[F[_], G[_[_]]](using F: Invariant[F], G: InvariantKC[G]): InvariantKC[[H[_]] =>> F[G[H]]] with
    extension [A[_], C](fa: F[G[A]])
      override def imapK[B[_]](f: A :~>: B)(g: B :~>: A): F[G[B]] = fa.imap(_.imapK(f)(g))(_.imapK(g)(f))

  given composeId[F[_], X](using F: Invariant[F]): InvariantKC[[H[_]] =>> F[H[X]]] = composeCats[F, IdFC[X]]

type InvariantKC[F[_[_]]] = InvariantK[IgnoreC[F]]