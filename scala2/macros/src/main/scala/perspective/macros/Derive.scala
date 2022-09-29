package perspective.macros

import scala.language.experimental.macros

import perspective._

object Derive {

  def functorK[F[_[_], _]]: FunctorK[F] = macro DeriveMacros.perspectiveFunctorK[F]
  def applyK[F[_[_], _]]: ApplyK[F] = macro DeriveMacros.perspectiveApplyK[F]
  def applicativeK[F[_[_], _]]: ApplicativeK[F] = macro DeriveMacros.applicativeK[F]
  def foldableK[F[_[_], _]]: FoldableK[F] = macro DeriveMacros.foldableK[F]
  def traverseK[F[_[_], _]]: TraverseK[F] = macro DeriveMacros.traverseK[F]
  def distributiveK[F[_[_], _]]: DistributiveK[F] = macro DeriveMacros.distributiveK[F]
  def representableK[F[_[_], _], Rep[_]]: RepresentableK[F] { type RepresentationK[A] = Rep[A] } =
    macro DeriveMacros.representableK[F]

  def allK[F[_[_], _], Rep[_]]: TraverseK[F] with RepresentableK[F] { type RepresentationK[A] = Rep[A] } =
    macro DeriveMacros.allK[F]

  def functorKC[F[_[_]]]: FunctorKC[F] = macro DeriveMacros.perspectiveFunctorKC[F]
  def applyKC[F[_[_]]]: ApplyKC[F] = macro DeriveMacros.perspectiveApplyKC[F]
  def applicativeKC[F[_[_]]]: ApplicativeKC[F] = macro DeriveMacros.applicativeKC[F]
  def foldableKC[F[_[_]]]: FoldableKC[F] = macro DeriveMacros.foldableKC[F]
  def traverseKC[F[_[_]]]: TraverseKC[F] = macro DeriveMacros.traverseKC[F]
  def distributiveKC[F[_[_]]]: DistributiveKC[F] = macro DeriveMacros.distributiveKC[F]
  def representableKC[F[_[_]], Rep[_]]: RepresentableKC[F] { type RepresentationK[A] = Rep[A] } =
    macro DeriveMacros.representableKC[F]

  def allKC[F[_[_]], Rep[_]]: TraverseKC[F] with RepresentableKC[F] { type RepresentationK[A] = Rep[A] } =
    macro DeriveMacros.allKC[F]

  type Names[A]           = Const[List[String], A]
  type Implicits[A[_], Z] = A[Z]

  def names[F[_[_], _]]: F[Names, _] = macro DeriveMacros.names[F]
  def namesC[F[_[_]]]: F[Names] = macro DeriveMacros.namesC[F]

  def withImplicits[F[_[_], _], A[_], C]: F[A, C] = macro DeriveMacros.withProductImplicits[F, A, C]
  def withImplicitsC[F[_[_]], A[_]]: F[A] = macro DeriveMacros.withProductImplicitsC[F, A]

  def namesWithImplicits[F[_[_], _], A[_], C]: F[Tuple2K[Names, A, *], C] =
    macro DeriveMacros.namesWithProductImplicits[F, A, C]
  def namesWithImplicitsC[F[_[_]], A[_]]: F[Tuple2K[Names, A, *]] =
    macro DeriveMacros.namesWithProductImplicitsC[F, A]
}
