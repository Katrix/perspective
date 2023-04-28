package perspective.derivation

import scala.annotation.tailrec
import scala.compiletime.*
import scala.deriving.*
import scala.reflect.ClassTag

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
    arr.asInstanceOf[IArray[Object]]
  }

  given productKInstance[T <: Tuple](
      using size: ValueOf[Tuple.Size[T]]
  ): RepresentableKC[ProductKPar[T]] with TraverseKC[ProductKPar[T]] with
    type RepresentationK[_] = Finite[Tuple.Size[T]]

    override def indicesK[C]: ProductK[RepresentationK, T] =
      ArrayProduct(iArrayOf(size.value)(_.asInstanceOf[Object]))

    extension [A[_], C](fa: ProductK[A, T])
      override def foldLeftK[B](b: B)(f: B => A ~>#: B): B =
        fa.productIterator.foldLeft(b) { (acc, v) =>
          val withAcc = f(acc)
          withAcc(v.asInstanceOf[A[Any]])
        }

      override def traverseK[G[_]: Applicative, B[_]](f: A ~>: Compose2[G, B]): G[ProductK[B, T]] =
        val it = fa.productIterator.asInstanceOf[Iterator[A[Any]]]
        val G  = summon[Applicative[G]]

        @tailrec
        def inner(acc: G[List[B[Any]]]): G[ProductK[B, T]] =
          if (it.hasNext) {
            val obj = it.next()
            inner(G.map2(f(obj), acc)((v, a) => v :: a))
          } else G.map(acc)(a => ArrayProduct.ofArrayUnsafe(a.reverseIterator.toArray.asInstanceOf[Array[Object]]))

        inner(G.pure(List.empty[B[Any]]))

      override def indexK[Z](i: RepresentationK[Z]): A[Z] =
        fa.productElement(i.value).asInstanceOf[A[Z]]

    def tabulateK[A[_], C](f: RepresentationK ~>: A): ProductK[A, T] =
      // If the given is used, we already know that size > 0
      given (Finite.NotZero[Tuple.Size[T]] =:= true) =
        <:<.refl[Boolean].asInstanceOf[Finite.NotZero[Tuple.Size[T]] =:= true]
      val arr = iArrayOf(size.value)(i => f(i.asInstanceOf[Finite[Tuple.Size[T]]]).asInstanceOf[Object])
      ArrayProduct(arr)
  end productKInstance

  inline given gatherImplicits[F[_], T <: Tuple]: ProductK[F, T] =
    ofProductUnsafe(ArrayProduct(Helpers.summonAllToObjectIArray[T, F]))
