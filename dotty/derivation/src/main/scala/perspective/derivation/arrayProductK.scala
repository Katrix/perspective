package perspective.derivation

import scala.annotation.tailrec
import scala.compiletime.*
import scala.deriving.*

import cats.syntax.all.*
import cats.{Applicative, Functor, Monoid}
import perspective.*

opaque type ArrayProductK[F[_], T <: Tuple] = IArray[Object]
type ArrayProductKPar[T <: Tuple]           = [F[_]] =>> ArrayProductK[F, T]
object ArrayProductK {

  /** Construct an [[ArrayProductK]] from a mapped tuple. */
  inline def of[F[_], T <: Tuple](t: Helpers.TupleMap[T, F]): ArrayProductK[F, T] = t.toIArray

  /** Access the mapped tuple. */
  extension [F[_], T <: Tuple](p: ArrayProductK[F, T])
    inline def tuple: Helpers.TupleMap[T, F] = Tuple.fromIArray(p).asInstanceOf[Helpers.TupleMap[T, F]]

  private inline def iArrayOf(inline size: Int)(inline f: Int => Object): IArray[Object] = {
    val arr = new Array[Object](size)
    var i: Int = 0
    while (i < size) {
      arr(i) = f(i)
      i += 1
    }
    arr.asInstanceOf[IArray[Object]]
  }

  given arrayProductKInstance[T <: Tuple](
      using size: ValueOf[Tuple.Size[T]]
  ): RepresentableKC[ArrayProductKPar[T]] with TraverseKC[ArrayProductKPar[T]] with
    type RepresentationK[_] = Finite[Tuple.Size[T]]

    override def indicesK[C]: ArrayProductK[RepresentationK, T] =
      iArrayOf(size.value)(i => i.asInstanceOf[Object]).asInstanceOf[ArrayProductK[RepresentationK, T]]

    extension [A[_], C](fa: ArrayProductK[A, T])
      override def foldLeftK[B](b: B)(f: B => A ~>#: B): B =
        fa.foldLeft(b) { (acc, v) =>
          val withAcc = f(acc)
          withAcc(v.asInstanceOf[A[Any]])
        }

      override def traverseK[G[_]: Applicative, B[_]](f: A ~>: Compose2[G, B]): G[ArrayProductK[B, T]] =
        val it = fa.iterator.asInstanceOf[Iterator[A[Any]]]
        val G = summon[Applicative[G]]

        @tailrec
        def inner(acc: G[List[B[Any]]]): G[ArrayProductK[B, T]] =
          if (it.hasNext) {
            val obj = it.next()
            inner(G.map2(acc, f(obj))((a, v) => v :: a))
          } else G.map(acc)(a => a.toArray.asInstanceOf[ArrayProductK[B, T]])

        inner(G.pure(List.empty[B[Any]]))

      override def indexK[Z](i: RepresentationK[Z]): A[Z] =
        fa(i.value).asInstanceOf[A[Z]]

    def tabulateK[A[_], C](f: RepresentationK ~>: A): ArrayProductK[A, T] =
      // If the given is used, we already know that size > 0
      given (Finite.NotZero[Tuple.Size[T]] =:= true) =
        <:<.refl[Boolean].asInstanceOf[Finite.NotZero[Tuple.Size[T]] =:= true]

      val arr = iArrayOf(size.value)(i => f(i.asInstanceOf[Finite[Tuple.Size[T]]]).asInstanceOf[Object])
      arr.asInstanceOf[ArrayProductK[A, T]]
  end arrayProductKInstance

  inline given gatherImplicits[F[_], T <: Tuple]: ArrayProductK[F, T] =
    inline Helpers.summonAllToIArray[T, F] match {
      case res: Array[Object] => res.asInstanceOf[IArray[Object]]
      case other              => iArrayOf(other.length)(i => other(i).asInstanceOf[Object])
    }

}
