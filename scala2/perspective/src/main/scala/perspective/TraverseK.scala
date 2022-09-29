package perspective

import cats.Applicative
import cats.data.State

trait TraverseK[F[_[_], _]] extends FunctorK[F] with FoldableK[F] {

  def traverseK[G[_]: Applicative, A[_], B[_], C](fa: F[A, C])(f: A ~>: Compose2[G, B, *]): G[F[B, C]]

  def sequenceK[G[_]: Applicative, A[_], C](fga: F[Compose2[G, A, *], C]): G[F[A, C]] =
    traverseK(fga)(FunctionK.identity)(Applicative[G])

  override def mapK[A[_], B[_], C](fa: F[A, C])(f: A ~>: B): F[B, C] =
    traverseK[cats.Id, A, B, C](fa)(f)

  def traverseIdK[G[_]: Applicative, A[_], C](fa: F[A, C])(f: A ~>: G): G[F[cats.Id, C]] =
    traverseK[G, A, cats.Id, C](fa)(f)

  def sequenceIdK[A[_]: Applicative, C](fa: F[A, C]): A[F[cats.Id, C]] = sequenceK[A, cats.Id, C](fa)

  def scanLeft[A, B[_], C](fa: F[Const[A, *], C])(zero: Unit #~>: B, f: Tuple2K[B, Const[A, *], *] ~>: B): F[B, C] =
    scanLeftK[Const[A, *], B, C](fa)(zero, f)

  // Adapted from https://stackoverflow.com/questions/47911415/scala-cats-or-scalaz-typeclass-scanleft-like
  def scanLeftK[A[_], B[_], C](fa: F[A, C])(zero: Unit #~>: B, f: Tuple2K[B, Const[A[_], *], *] ~>: B): F[B, C] = {
    val generate = new FunctionK[A, Compose2[State[Unit #~>: B, *], B, *]] {
      override def apply[Z](fa: A[Z]): Compose2[State[Unit #~>: B, *], B, Z] =
        for {
          prevF <- State.get[Unit #~>: B]
          nextF = Lambda[Const[Unit, *] ~>: B](_ => f((prevF(()), fa)))
          _ <- State.set[Unit #~>: B](nextF)
        } yield nextF(())
    }

    traverseK[State[Unit #~>: B, *], A, B, C](fa)(generate).runA(zero).value
  }

  def numbered[C](implicit F: ApplicativeK[F]): F[Const[Int, *], C] =
    scanLeft[Unit, Const[Int, *], C](F.unitK[C])(
      FunctionK.const(0),
      FunctionK.liftConst((t: (Int, Unit)) => t._1 + 1)
    )
}
