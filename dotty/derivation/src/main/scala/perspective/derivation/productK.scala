package perspective.derivation

import scala.annotation.tailrec
import scala.compiletime.*
import scala.deriving.*

import cats.syntax.all.*
import cats.{Applicative, Functor, Monoid}
import perspective.*

/**
  * A structure allowing higher kinded operations over a normal tuple.
  * Equivalent but easier to work with than [[Tuple.Map]]. Also supports getting
  * given instances of itself provided instances for all the types exists..
  * @tparam F
  *   The current type constructor of the value.
  * @tparam T
  *   The tuple that we are working with.
  */
opaque type ProductK[F[_], T <: Tuple] = Tuple.Map[T, F]

/** A partially applied [[ProductK]] */
type ProductKPar[T <: Tuple] = [F[_]] =>> ProductK[F, T]
object ProductK extends ProductKGather:
  /** Construct a [[ProductK]] from a mapped tuple. */
  inline def of[F[_], T <: Tuple](t: Tuple.Map[T, F]): ProductK[F, T] = t

  /** Access the mapped tuple.. */
  extension [F[_], T <: Tuple](p: ProductK[F, T]) inline def tuple: Tuple.Map[T, F] = p

  given productKInstance[T <: Tuple](
      using size: ValueOf[Tuple.Size[T]]
  ): RepresentableKC[ProductKPar[T]] with TraverseKC[ProductKPar[T]] with
    type RepresentationK[_] = Finite[Tuple.Size[T]]

    extension [A[_], C](fa: ProductK[A, T])
      override def foldLeftK[B](b: B)(f: B => A ~>#: B): B =
        fa.productIterator.foldLeft(b) { (acc, v) =>
          val withAcc = f(acc)
          withAcc(v.asInstanceOf[A[Any]])
        }

      override def traverseK[G[_]: Applicative, B[_]](f: A ~>: Compose2[G, B]): G[ProductK[B, T]] =
        val it = fa.productIterator.asInstanceOf[Iterator[A[Any]]]

        @tailrec
        def inner(acc: G[List[B[Any]]]): G[ProductK[B, T]] =
          if (it.hasNext) {
            val obj = it.next()
            inner(Applicative[G].map2(f(obj), acc)((v, a) => v :: a))
          } else Applicative[G].map(acc)(a => Tuple.fromArray(a.reverseIterator.toArray).asInstanceOf[ProductK[B, T]])

        inner(Applicative[G].pure(List.empty[B[Any]]))

      override def indexK: RepresentationK ~>: A =
        [Z] => (rep: RepresentationK[Z]) => fa.productElement(rep.value).asInstanceOf[A[Z]]

    def tabulateK[A[_], C](f: RepresentationK ~>: A): ProductK[A, T] =
      // If the given is used, we already know that size > 0
      given (Finite.NotZero[Tuple.Size[T]] =:= true) =
        <:<.refl[Boolean].asInstanceOf[Finite.NotZero[Tuple.Size[T]] =:= true]
      val arr = IArray.tabulate(size.value)(i => f(Finite(size.value, i)))
      Tuple.fromIArray(arr).asInstanceOf[ProductK[A, T]]
  end productKInstance

trait ProductKGather:
  self: ProductK.type =>
  inline given gatherImplicits[F[_], T <: Tuple]: ProductK[F, T] =
    self.of(Helpers.summonAllOptimized[Tuple.Map[T, F]].asInstanceOf[Tuple.Map[T, F]])
