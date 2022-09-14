package perspective.syntax

import scala.language.implicitConversions

import perspective.{RepresentableK, RepresentableKC, ~>:}

trait RepresentableKSyntax {

  implicit def perspectiveRepresentableKCFAOps[F[_[_]]: RepresentableKC, A[_], RepresentationK[_]](
      fa: F[A]
  )(implicit F: RepresentableKC.Aux[F, RepresentationK]): RepresentableKCFAOps[F, A, RepresentationK] =
    new RepresentableKCFAOps[F, A, RepresentationK](fa)

  implicit def perspectiveRepresentableKFAOps[F[_[_], _]: RepresentableK, A[_], RepresentationK[_], C](
      fa: F[A, C]
  )(implicit F: RepresentableK.Aux[F, RepresentationK]): RepresentableKFAOps[F, A, RepresentationK, C] =
    new RepresentableKFAOps[F, A, RepresentationK, C](fa)
}

final class RepresentableKCFAOps[F[_[_]], A[_], RepresentationK[_]](private val fa: F[A]) extends AnyVal {

  def indexKC(implicit F: RepresentableKC.Aux[F, RepresentationK]): RepresentationK ~>: A = F.indexK(fa)

  def indexKC[X](rep: RepresentationK[X])(implicit F: RepresentableKC.Aux[F, RepresentationK]): A[X] = F.indexK(fa)(rep)
}

final class RepresentableKFAOps[F[_[_], _], A[_], RepresentationK[_], C](private val fa: F[A, C]) extends AnyVal {

  def indexKC(implicit F: RepresentableK.Aux[F, RepresentationK]): RepresentationK ~>: A = F.indexK(fa)

  def indexKC[X](rep: RepresentationK[X])(implicit F: RepresentableK.Aux[F, RepresentationK]): A[X] = F.indexK(fa)(rep)
}
