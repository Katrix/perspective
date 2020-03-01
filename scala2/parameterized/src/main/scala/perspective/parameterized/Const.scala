package perspective.parameterized

import cats.kernel.Monoid
import cats.{Applicative, Apply, Eval, Foldable, Functor, Semigroup}

object Const {
  type Const[A, B]

  def unit[B]: Const[Unit, B] = apply(())

  @inline def apply[A, B](value: A): Const[A, B] = value.asInstanceOf[Const[A, B]]

  //TODO: Prioritize typeclasses
  implicit class ConstOps[A, B](private val const: Const[A, B]) extends AnyVal {
    def value: A = const.asInstanceOf[A]

    @inline def retype[NewB]: Const[A, NewB] = const.asInstanceOf[Const[A, NewB]]
  }

  implicit def constFunctorInstance[X]: Functor[Const[X, *]] = new Functor[Const[X, *]] {
    override def map[A, B](fa: Const[X, A])(f: A => B): Const[X, B] = fa.retype
  }

  implicit def constApplyInstance[X](implicit semigroup: Semigroup[X]): Apply[Const[X, *]] = new Apply[Const[X, *]] {
    override def ap[A, B](ff: Const[X, A => B])(fa: Const[X, A]): Const[X, B] =
      apply(semigroup.combine(ff.value, fa.value))

    override def map[A, B](fa: Const[X, A])(f: A => B): Const[X, B] = fa.retype
  }

  implicit def constApplicativeInstance[X](implicit monoid: Monoid[X]): Applicative[Const[X, *]] =
    new Applicative[Const[X, *]] {
      override def pure[A](x: A): Const[X, A] = apply(monoid.empty)

      override def ap[A, B](ff: Const[X, A => B])(fa: Const[X, A]): Const[X, B] =
        apply(monoid.combine(ff.value, fa.value))
    }

  implicit def constFoldableInstance[X]: Foldable[Const[X, *]] = new Foldable[Const[X, *]] {
    override def foldLeft[A, B](fa: Const[X, A], b: B)(f: (B, A) => B): B = b

    override def foldRight[A, B](fa: Const[X, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
  }
}
