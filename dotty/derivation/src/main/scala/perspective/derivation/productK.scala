package perspective.derivation

import scala.annotation.tailrec
import scala.compiletime.*
import scala.deriving.*
import scala.reflect.ClassTag

import cats.kernel.{BoundedEnumerable, Order}
import cats.syntax.all.*
import cats.{Applicative, Functor, Monoid}
import perspective.*

/**
  * A structure allowing higher kinded operations over a normal tuple.
  * Equivalent but easier to work with than [[Tuple.Map]]. Also supports getting
  * given instances of itself provided instances for all the types exists.
  * @tparam F
  *   The current type constructor of the value.
  * @tparam T
  *   The tuple that we are working with.
  */
opaque type ProductK[F[_], T <: Tuple] = Product

/** A partially applied [[ProductK]] */
type ProductKPar[T <: Tuple] = [F[_]] =>> ProductK[F, T]
object ProductK:
  /** Construct a [[ProductK]] from a mapped tuple. */
  inline def ofTuple[F[_], T <: Tuple](t: Helpers.TupleMap[T, F]): ProductK[F, T] = t

  /** Construct a [[ProductK]] from a scala mapped tuple. */
  inline def ofScalaTuple[F[_], T <: Tuple](t: Tuple.Map[T, F]): ProductK[F, T] = t

  /** Construct a [[ProductK]] from a product, with no checks. Unsafe!! */
  inline def ofProductUnsafe[F[_], T <: Tuple](p: Product): ProductK[F, T] = p

  /** Construct a [[ProductK]] from a product with a mirror. */
  inline def ofProduct[A <: Product](p: A)(using m: Mirror.Of[A]): ProductK[Id, m.MirroredElemTypes] = p

  /** Access the mapped tuple. */
  extension [F[_], T <: Tuple](p: ProductK[F, T])
    def tuple: Helpers.TupleMap[T, F] = p match
      case tuple: Tuple     => tuple.asInstanceOf[Helpers.TupleMap[T, F]]
      case product: Product => Tuple.fromProduct(product).asInstanceOf[Helpers.TupleMap[T, F]]

    def scalaTuple: Tuple.Map[T, F] = tuple.asInstanceOf[Tuple.Map[T, F]]

    inline def product: Product = p

  private inline def iArrayOf(inline size: Int)(inline f: Int => Object): IArray[Object] = {
    val arr    = new Array[Object](size)
    var i: Int = 0
    while (i < size) {
      arr(i) = f(i)
      i += 1
    }
    Helpers.unsafeArrayToIArray(arr)
  }

  given productKInstance[T <: Tuple](
      using typeLength: TypeLength[T]
  ): (BoundedRepresentableKC.Aux[ProductKPar[T], [_] =>> Finite[typeLength.Length]] & TraverseKC[ProductKPar[T]]) =
    new BoundedRepresentableKC[ProductKPar[T]] with TraverseKC[ProductKPar[T]]:
      type RepresentationK[_] = Finite[typeLength.Length]

      private inline def objToHK[A[_]](obj: Any): A[Any] = obj.asInstanceOf[A[Any]]

      override def indicesK[C]: ProductK[RepresentationK, T] =
        ArrayProduct(iArrayOf(typeLength.length)(i => Int.box(i)))

      extension [A[_], C](fa: ProductK[A, T])
        override def foldLeftK[B](b: B)(f: B => A :~>#: B): B =
          fa.productIterator.foldLeft(b) { (acc, v) =>
            val withAcc = f(acc)
            withAcc(objToHK[A](v))
          }

        override def foldRightK[B](b: B)(f: A :~>#: (B => B)): B =
          fa.productIterator.foldRight(b) { (v, acc) =>
            val withV = f(objToHK[A](v))
            withV(acc)
          }

        override def traverseK[G[_]: Applicative, B[_]](f: A :~>: Compose2[G, B]): G[ProductK[B, T]] =
          val it = (fa.productIterator: Iterator[Any]).asInstanceOf[Iterator[A[Any]]]
          val G  = summon[Applicative[G]]

          @tailrec
          def inner(acc: G[List[B[Any]]]): G[ProductK[B, T]] =
            if (it.hasNext) {
              val obj = it.next()
              inner(G.map2(acc, f(obj))((a, v) => v :: a))
            } else
              G.map(acc)(a =>
                ArrayProduct.ofArrayUnsafe((a.reverseIterator.toArray: Array[B[Any]]).asInstanceOf[Array[Object]])
              )

          inner(G.pure(List.empty[B[Any]]))

        override def indexK[Z](i: RepresentationK[Z]): A[Z] =
          (fa.productElement(i.value): Any).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK :~>: A): ProductK[A, T] =
        val arr = iArrayOf(typeLength.length)(i => Helpers.boxAny(f(Finite.unsafeApply(i))))
        ArrayProduct(arr)

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
  end productKInstance

  inline given gatherImplicits[F[_], T <: Tuple]: ProductK[F, T] =
    ofProductUnsafe(ArrayProduct(Helpers.summonAllToObjectIArray[T, F]))
