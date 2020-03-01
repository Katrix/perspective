package perspective.syntax

import perspective.{RepresentableK, RepresentableKC, ~>:}

import scala.language.implicitConversions

trait RepresentableKSyntax extends RepresentableK.ToRepresentableKOps {

  implicit def perspectiveRepresentableKCFAOps[F[_[_]]: RepresentableKC, A[_], RepresentationK[_]](
      fa: F[A]
  )(implicit F: RepresentableKC.Aux[F, RepresentationK]): RepresentableKCFAOps[F, A, RepresentationK] =
    new RepresentableKCFAOps[F, A, RepresentationK](fa)
}

final class RepresentableKCFAOps[F[_[_]], A[_], RepresentationK[_]](private val fa: F[A]) extends AnyVal {

  def indexKC(implicit F: RepresentableKC.Aux[F, RepresentationK]): RepresentationK ~>: A = F.indexK(fa)

  def indexKC[X](rep: RepresentationK[X])(implicit F: RepresentableKC.Aux[F, RepresentationK]): A[X] = F.indexK(fa)(rep)
}
