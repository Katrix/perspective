package perspective.parameterized

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

  def const[F[_], A](a: A): F ~>: Const[A, *] = new FunctionK[F, Const[A, *]] {
    override def apply[Z](fa: F[Z]): Const[A, Z] = Const(a)
  }

  def liftFromContravariant[F[+_], A](f: F[Any] => A): F ~>: Const[A, *] = λ[F ~>: Const[A, *]](fa => Const(f(fa)))
  def liftToContravariant[F[-_], A](f: A => F[Any]): Const[A, *] ~>: F   = λ[Const[A, *] ~>: F](a => f(a.value))
  def liftFromCovariant[F[-_], A](f: F[Nothing] => A): F ~>: Const[A, *] = λ[F ~>: Const[A, *]](fa => Const(f(fa)))
  def liftToCovariant[F[+_], A](f: A => F[Nothing]): Const[A, *] ~>: F   = λ[Const[A, *] ~>: F](a => f(a.value))

  def liftConst[A, B](f: A => B): Const[A, *] ~>: Const[B, *] = λ[Const[A, *] ~>: Const[B, *]](a => Const(f(a.value)))
}
