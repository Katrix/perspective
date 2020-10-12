package perspective

import cats.Functor
import cats.syntax.all._

import scala.language.implicitConversions

trait RepresentableK[F[_[_], _]] extends MonadK[F] with DistributiveK[F]:

  type RepresentationK[_]

  def indexK[A[_], C](fa: F[A, C]): RepresentationK ~>: A

  def tabulateK[A[_], C](f: RepresentationK ~>: A): F[A, C]

  extension[A[_], C](fa: F[A, C]) def indexK: RepresentationK ~>: A = indexK(fa)

  extension[A[_], C](a: ValueK[A]) override def pure: F[A, C] = 
    tabulateK([Z] => (r: RepresentationK[Z]) => a[Z]())

  extension[A[_], B[_], C](fa: F[A, C]) override def flatMapK(f: A ~>: F[B, *]): F[B, C] =
    tabulateK([Z] => (r: RepresentationK[Z]) => indexK(f(indexK(fa)(r)))(r))

  extension[G[_]: Functor, A[_], C](gfa: G[F[A, C]]) override def cosequenceK: F[Compose2[G, A], C] =
    tabulateK([Z] => (r: RepresentationK[Z]) => gfa.map(fa => indexK(fa)(r)))

  extension[A[_], B[_], C](fa: F[A, C]) override def mapK (f: A ~>: B): F[B, C] =
    tabulateK([Z] => (r: RepresentationK[Z]) => f(indexK(fa)(r)))

  extension[A[_], B[_], Z[_], C](fa: F[A, C]) override def map2K(fb: F[B, C])(f: Tuple2K[A, B] ~>: Z): F[Z, C] =
    tabulateK([Z] => (r: RepresentationK[Z]) => f((indexK(fa)(r), indexK(fb)(r))))

object RepresentableK:
  type Aux[F[_[_], _], RepresentationK0[_]] = RepresentableK[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }

type RepresentableKC[F[_[_]]] = RepresentableK[IgnoreC[F]]
object RepresentableKC:
  type Aux[F[_[_]], RepresentationK0[_]] = RepresentableKC[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }