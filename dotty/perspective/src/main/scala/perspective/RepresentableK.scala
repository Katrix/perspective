package perspective

import cats.Functor
import cats.syntax.all._

import scala.language.implicitConversions

trait RepresentableK[F[_[_], _]] extends MonadK[F] with DistributiveK[F]:

  type RepresentationK[_]

  def tabulateK[A[_], C](f: RepresentationK ~>: A): F[A, C]

  extension[A[_], C](fa: F[A, C]) def indexK: RepresentationK ~>: A
  
  def indicesK[C]: F[RepresentationK, C] = tabulateK(FunctionK.identity)

  extension[A[_], C](a: ValueK[A]) override def pure: F[A, C] = 
    tabulateK([Z] => (r: RepresentationK[Z]) => a[Z]())

  extension[A[_], B[_], C](fa: F[A, C]) override def flatMapK(f: A ~>: F[B, *]): F[B, C] =
    tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r)).indexK(r))
  
  extension[G[_]: Functor, A[_], C](gfa: G[F[A, C]]) override def cosequenceK: F[Compose2[G, A], C] =
    tabulateK([Z] => (r: RepresentationK[Z]) => gfa.map(fa => fa.indexK(r)))

  extension[A[_], B[_], C](fa: F[A, C]) override def mapK (f: A ~>: B): F[B, C] =
    tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r)))

  extension[A[_], B[_], Z[_], C](fa: F[A, C]) override def map2K(fb: F[B, C])(f: [X] => (A[X], B[X]) => Z[X]): F[Z, C] =
    tabulateK([Z] => (r: RepresentationK[Z]) => f(fa.indexK(r), fb.indexK(r)))

object RepresentableK:
  type Aux[F[_[_], _], RepresentationK0[_]] = RepresentableK[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }

type RepresentableKC[F[_[_]]] = RepresentableK[IgnoreC[F]]
object RepresentableKC:
  type Aux[F[_[_]], RepresentationK0[_]] = RepresentableKC[F] {
    type RepresentationK[A] = RepresentationK0[A]
  }