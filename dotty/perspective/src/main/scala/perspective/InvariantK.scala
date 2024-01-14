package perspective

import cats.Invariant
import cats.syntax.all.*

trait InvariantK[F[_[_], _]]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Invariant.imap]]. */
    def imapK[B[_]](f: A :~>: B)(g: B :~>: A): F[B, C]

object InvariantK:
  given idInstanceC[A]: InvariantKC[IdFC[A]] = instances.idInstanceC[A]

  given composeCatsOutside[F[_], G[_[_]]](using F: Invariant[F], G: InvariantKC[G]): InvariantKC[[H[_]] =>> F[G[H]]]
    with
    extension [A[_], C](fa: F[G[A]])
      override def imapK[B[_]](f: A :~>: B)(g: B :~>: A): F[G[B]] = fa.imap(_.imapK(f)(g))(_.imapK(g)(f))

  given composeId[F[_], X](using F: Invariant[F]): InvariantKC[[H[_]] =>> F[H[X]]] =
    composeCatsOutside[F, IdFC[X]]

  given composeCatsInside[F[_[_]], G[_]](
      using F: InvariantKC[F],
      G: Invariant[G]
  ): InvariantKC[[H[_]] =>> F[Compose2[G, H]]] with {
    extension [A[_], C](fga: F[Compose2[G, A]])
      override def imapK[B[_]](f: A :~>: B)(g: B :~>: A): F[Compose2[G, B]] =
        val f2: Compose2[G, A] :~>: Compose2[G, B] = [X] => (ga: G[A[X]]) => ga.imap(a => f(a))(b => g(b))
        val g2: Compose2[G, B] :~>: Compose2[G, A] = [X] => (gb: G[B[X]]) => gb.imap(b => g(b))(a => f(a))
        F.imapK(fga)(f2)(g2)
  }

type InvariantKC[F[_[_]]] = InvariantK[IgnoreC[F]]
