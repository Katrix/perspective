package perspective.derivation

import cats.Applicative
import perspective._

import scala.annotation.tailrec
import scala.deriving._
import scala.compiletime._

opaque type ProductK[F[_], T <: Tuple] = Tuple.Map[T, F]
object ProductK extends ProductKGather:
  def of[F[_], T <: Tuple](t: Tuple.Map[T, F]): ProductK[F, T] = t
  
  extension [F[_], T <: Tuple](p: ProductK[F, T]) def tuple: Tuple.Map[T, F] = p
  
  given productKInstance[T <: Tuple](using size: ValueOf[Tuple.Size[T]], ev: Finite.NotZero[Tuple.Size[T]] =:= true) as RepresentableKC[[F[_]] =>> ProductK[F, T]], TraverseKC[[F[_]] =>> ProductK[F, T]]:
    type RepresentationK[_] = Finite[Tuple.Size[T]]
    
    extension[A[_], B, C](fa: ProductK[A, T]) override def foldLeftK(b: B)(f: B => A ~>#: B): B = 
      fa.productIterator.foldLeft(b) { (acc, v) =>
        val withAcc = f(acc)
        withAcc(v.asInstanceOf[A[Any]])
      }

    extension[G[_]: Applicative, A[_], B[_], C](fa: ProductK[A, T]) def traverseK(f: A ~>: Compose2[G, B]): G[ProductK[B, T]] =
      val it = fa.productIterator.asInstanceOf[Iterator[A[Any]]]
    
      @tailrec
      def inner(acc: G[List[B[Any]]]): G[ProductK[B, T]] =
        if (it.hasNext) {
          val obj = it.next()
          inner(Applicative[G].map2(f(obj), acc)((v, a) => v :: a))
        } else
          Applicative[G].map(acc)(a => Tuple.fromArray(a.reverseIterator.toArray).asInstanceOf[ProductK[B, T]])

      inner(Applicative[G].pure(List.empty[B[Any]]))

    def tabulateK[A[_], C](f: RepresentationK ~>: A): ProductK[A, T] = 
      val arr = IArray.tabulate(size.value)(i => f(Finite(size.value, i)))
      Tuple.fromIArray(arr).asInstanceOf[ProductK[A, T]]

    extension[A[_], C](fa: ProductK[A, T]) def indexK: RepresentationK ~>: A = 
      [Z] => (rep: RepresentationK[Z]) => fa.productElement(rep.value).asInstanceOf[A[Z]]

trait ProductKGather:
  inline given gatherImplicits[F[_], T <: Tuple] as ProductK[F, T] = summonAll[Tuple.Map[T, F]].asInstanceOf[ProductK[F, T]]
