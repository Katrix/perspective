package perspective

import cats.Contravariant
import cats.syntax.all.*

trait ContravariantK[F[_[_], _]] extends InvariantK[F]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Contravariant.contramap]]. */
    def contramapK[B[_]](f: B :~>: A): F[B, C]

    override def imapK[B[_]](f: A :~>: B)(g: B :~>: A): F[B, C] = contramapK(g)

object ContravariantK:
  given composeCats[F[_], G[_[_]]](using F: Contravariant[F], G: FunctorKC[G]): ContravariantKC[[H[_]] =>> F[G[H]]] with
    extension [A[_], C](fa: F[G[A]])
      override def contramapK[B[_]](f: B :~>: A): F[G[B]] = fa.contramap(_.mapK(f))

  given composeId[F[_], X](using F: Contravariant[F]): ContravariantKC[[H[_]] =>> F[H[X]]] = composeCats[F, IdFC[X]]

type ContravariantKC[F[_[_]]] = ContravariantK[IgnoreC[F]]