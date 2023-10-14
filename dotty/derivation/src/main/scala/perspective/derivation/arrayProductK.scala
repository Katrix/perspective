package perspective.derivation

import scala.annotation.tailrec
import scala.compiletime.*
import scala.deriving.*

import cats.kernel.{BoundedEnumerable, Order}
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
    val arr    = new Array[Object](size)
    var i: Int = 0
    while (i < size) {
      arr(i) = f(i)
      i += 1
    }
    Helpers.unsafeArrayToIArray(arr)
  }

  given arrayProductKInstance[T <: Tuple](
      using typeLength: TypeLength[T]
  ): (BoundedRepresentableKC.Aux[ArrayProductKPar[T], [_] =>> Finite[typeLength.Length]] &
    TraverseKC[ArrayProductKPar[T]]) = new BoundedRepresentableKC[ArrayProductKPar[T]]
    with TraverseKC[ArrayProductKPar[T]]:
    type RepresentationK[_] = Finite[typeLength.Length]

    private inline def objToHK[A[_]](obj: Object): A[Any] = obj.asInstanceOf[A[Any]]

    private inline def arrToArrProdK[A[_]](arr: Array[Object]): ArrayProductK[A, T] =
      Helpers.unsafeArrayToIArray(arr)

    override def indicesK[C]: ArrayProductK[RepresentationK, T] =
      iArrayOf(typeLength.length)(i => Int.box(i))

    extension [A[_], C](fa: ArrayProductK[A, T])
      override def foldLeftK[B](b: B)(f: B => A :~>#: B): B =
        fa.foldLeft(b) { (acc, v) =>
          val withAcc = f(acc)
          withAcc(objToHK[A](v))
        }

      override def foldRightK[B](b: B)(f: A :~>#: (B => B)): B =
        fa.foldRight(b) { (v, acc) =>
          val withV = f(objToHK[A](v))
          withV(acc)
        }

      override def traverseK[G[_]: Applicative, B[_]](f: A :~>: Compose2[G, B]): G[ArrayProductK[B, T]] =
        val it = (fa.iterator: Iterator[Any]).asInstanceOf[Iterator[A[Any]]]
        val G  = summon[Applicative[G]]

        @tailrec
        def inner(acc: G[List[B[Any]]]): G[ArrayProductK[B, T]] =
          if (it.hasNext) {
            val obj = it.next()
            inner(G.map2(acc, f(obj))((a, v) => v :: a))
          } else G.map(acc)(a => (a.toArray: Array[B[Any]]).asInstanceOf[IArray[Object]])

        inner(G.pure(List.empty[B[Any]]))

      override def indexK[Z](i: RepresentationK[Z]): A[Z] =
        (objToHK[A](fa(i.value)): A[Any]).asInstanceOf[A[Z]]

    def tabulateK[A[_], C](f: RepresentationK :~>: A): ArrayProductK[A, T] =
      iArrayOf(typeLength.length)(i => Helpers.boxAny(f(Finite.unsafeApply(i))))

    override val boundedRepresentableK: BoundedEnumerable[ReprWrapper[_]] = new BoundedEnumerable[ReprWrapper[_]]:
      private val instance = Finite.boundedEnumerable[typeLength.Length]

      override def order: Order[ReprWrapper[_]] = (x: ReprWrapper[_], y: ReprWrapper[_]) =>
        instance.order.compare(x.repr, y.repr)

      override def maxBound: ReprWrapper[_] = ReprWrapper(instance.maxBound)

      override def partialPrevious(a: ReprWrapper[_]): Option[ReprWrapper[_]] =
        instance.partialPrevious(a.repr).map(ReprWrapper.apply)

      override def partialNext(a: ReprWrapper[_]): Option[ReprWrapper[_]] =
        instance.partialNext(a.repr).map(ReprWrapper.apply)

      override def minBound: ReprWrapper[_] = ReprWrapper(instance.minBound)
  end arrayProductKInstance

  inline given gatherImplicits[F[_], T <: Tuple]: ArrayProductK[F, T] =
    Helpers.summonAllToObjectIArray[T, F]

}
