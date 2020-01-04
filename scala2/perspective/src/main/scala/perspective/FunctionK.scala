package perspective

import scala.language.implicitConversions

trait FunctionK[F[_], G[_]] { self =>

  def apply[Z](fa: F[Z]): G[Z]

  def compose[H[_]](f: H ~>: F): H ~>: G =
    λ[H ~>: G](fa => self(f(fa)))

  def andThen[H[_]](f: G ~>: H): F ~>: H =
    λ[F ~>: H](fa => f(self(fa)))
}
object FunctionK {

  def identity[F[_]]: F ~>: F = λ[F ~>: F](Predef.identity(_))

  def const[F[_], A](a: A): F ~>#: A = new FunctionK[F, Const[A]#λ] {
    override def apply[Z](fa: F[Z]): A = a
  }

  def liftFromContravariant[F[+_], A](f: F[Any] => A): F ~>#: A = λ[F ~>: Const[A]#λ](fa => f(fa))
  def liftToContravariant[F[-_], A](f: A => F[Any]): A #~>: F   = λ[Const[A]#λ ~>: F](a => f(a))
  def liftFromCovariant[F[-_], A](f: F[Nothing] => A): F ~>#: A = λ[F ~>: Const[A]#λ](fa => f(fa))
  def liftToCovariant[F[+_], A](f: A => F[Nothing]): A #~>: F   = λ[Const[A]#λ ~>: F](a => f(a))

  def liftConst[A, B](f: A => B): A #~>#: B = λ[Const[A]#λ ~>: Const[B]#λ](a => f(a))
}
