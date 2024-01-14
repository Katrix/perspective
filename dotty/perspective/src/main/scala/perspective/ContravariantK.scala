package perspective

import cats.Contravariant
import cats.syntax.all.*

trait ContravariantK[F[_[_], _]] extends InvariantK[F]:
  extension [A[_], C](fa: F[A, C])
    /** A higher kinded equivalent of [[cats.Contravariant.contramap]]. */
    def contramapK[B[_]](f: B :~>: A): F[B, C]

    override def imapK[B[_]](f: A :~>: B)(g: B :~>: A): F[B, C] = contramapK(g)

object ContravariantK:
  given composeCatsOutside[F[_], G[_[_]]](using F: Contravariant[F], G: FunctorKC[G]): ContravariantKC[[H[_]] =>> F[G[H]]] with
    extension [A[_], C](fa: F[G[A]])
      override def contramapK[B[_]](f: B :~>: A): F[G[B]] = fa.contramap(_.mapK(f))

  given composeId[F[_], X](using F: Contravariant[F]): ContravariantKC[[H[_]] =>> F[H[X]]] = composeCatsOutside[F, IdFC[X]]
  
  given composeCatsInside[F[_[_]], G[_]](using F: FunctorKC[F], G: Contravariant[G]): ContravariantKC[[H[_]] =>> F[Compose2[G, H]]] with {
    extension [A[_], C](fga: F[Compose2[G, A]])
      override def contramapK[B[_]](f: B :~>: A): F[Compose2[G, B]] =
        F.mapK(fga)([X] => (ga: G[A[X]]) => G.contramap[A[X], B[X]](ga)(b => f(b)))
  }

type ContravariantKC[F[_[_]]] = ContravariantK[IgnoreC[F]]