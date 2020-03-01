package perspective.macros

import perspective._

import scala.language.experimental.macros

object Derive {

  def functorK[F[_[_], _]]: FunctorK[F] = macro DeriveMacros.perspectiveFunctorK[F]
  def applyK[F[_[_], _]]: ApplyK[F] = macro DeriveMacros.perspectiveApplyK[F]
  def applicativeK[F[_[_], _]]: ApplicativeK[F] = macro DeriveMacros.applicativeK[F]
  def foldableK[F[_[_], _]]: FoldableK[F] = macro DeriveMacros.foldableK[F]
  def traverseK[F[_[_], _]]: TraverseK[F] = macro DeriveMacros.traverseK[F]
  def distributiveK[F[_[_], _]]: DistributiveK[F] = macro DeriveMacros.distributiveK[F]

  def allK[F[_[_], _]]: ApplicativeK[F] with TraverseK[F] with DistributiveK[F] =
    macro DeriveMacros.allK[F]

  def functorKC[F[_[_]]]: FunctorKC[F] = macro DeriveMacros.perspectiveFunctorKC[F]
  def applyKC[F[_[_]]]: ApplyKC[F] = macro DeriveMacros.perspectiveApplyKC[F]
  def applicativeKC[F[_[_]]]: ApplicativeKC[F] = macro DeriveMacros.applicativeKC[F]
  def foldableKC[F[_[_]]]: FoldableKC[F] = macro DeriveMacros.foldableKC[F]
  def traverseKC[F[_[_]]]: TraverseKC[F] = macro DeriveMacros.traverseKC[F]
  def distributiveKC[F[_[_]]]: DistributiveKC[F] = macro DeriveMacros.distributiveKC[F]

  def allKC[F[_[_]]]: ApplicativeKC[F] with TraverseKC[F] with DistributiveKC[F] =
    macro DeriveMacros.allKC[F]

  type Names = {
    type λ[A] = Const[List[String]]#λ[A]
  }
  type ProductImplicits[A[_]] = {
    type λ[Z] = A[Z]
  }
  type SumImplicits[A[_]] = {
    type λ[Z] = Map[String, A[Z]]
  }
  type ADTImplicits[A[_]] = {
    type λ[Z] = Either[ProductImplicits[A]#λ[Z], SumImplicits[A]#λ[Z]]
  }

  def names[F[_[_], _]]: F[Names#λ, _] = macro DeriveMacros.names[F]
  def namesC[F[_[_]]]: F[Names#λ] = macro DeriveMacros.namesC[F]

  def withProductImplicits[F[_[_], _], A[_], C]: F[A, C] = macro DeriveMacros.withProductImplicits[F, A, C]
  def withProductImplicitsC[F[_[_]], A[_]]: F[A] = macro DeriveMacros.withProductImplicitsC[F, A]

  def withSumImplicits[F[_[_], _], A[_], C]: F[SumImplicits[A]#λ, C] = macro DeriveMacros.withSumImplicits[F, A, C]
  def withSumImplicitsC[F[_[_]], A[_]]: F[SumImplicits[A]#λ] = macro DeriveMacros.withSumImplicitsC[F, A]

  def withADTImplicits[F[_[_], _], A[_], C]: F[ADTImplicits[A]#λ, C] = macro DeriveMacros.withADTImplicits[F, A, C]
  def withADTImplicitsC[F[_[_]], A[_]]: F[ADTImplicits[A]#λ] = macro DeriveMacros.withADTImplicitsC[F, A]

  def namesWithProductImplicits[F[_[_], _], A[_], C]: F[Tuple2K[Names#λ, A]#λ, C] =
    macro DeriveMacros.namesWithProductImplicits[F, A, C]
  def namesWithProductImplicitsC[F[_[_]], A[_]]: F[Tuple2K[Names#λ, A]#λ] =
    macro DeriveMacros.namesWithProductImplicitsC[F, A]

  def namesWithSumImplicits[F[_[_], _], A[_], C]: F[Tuple2K[Names#λ, SumImplicits[A]#λ]#λ, C] =
    macro DeriveMacros.namesWithSumImplicits[F, A, C]
  def namesWithSumImplicitsC[F[_[_]], A[_]]: F[Tuple2K[Names#λ, SumImplicits[A]#λ]#λ] =
    macro DeriveMacros.namesWithSumImplicitsC[F, A]

  def namesWithADTImplicits[F[_[_], _], A[_], C]: F[Tuple2K[Names#λ, ADTImplicits[A]#λ]#λ, C] =
    macro DeriveMacros.namesWithADTImplicits[F, A, C]
  def namesWithADTImplicitsC[F[_[_]], A[_]]: F[Tuple2K[Names#λ, ADTImplicits[A]#λ]#λ] =
    macro DeriveMacros.namesWithADTImplicitsC[F, A]
}
