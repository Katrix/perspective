package perspective.derivation

import cats.Applicative
import perspective._

import scala.annotation.tailrec
import scala.deriving._
import scala.compiletime._

opaque type ProductK[F[_], T <: Tuple] = Tuple.Map[T, F]
type ProductKPar[T <: Tuple] = [F[_]] =>> ProductK[F, T]
object ProductK extends ProductKGather:
  def of[F[_], T <: Tuple](t: Tuple.Map[T, F]): ProductK[F, T] = t
  
  extension [F[_], T <: Tuple](p: ProductK[F, T]) def tuple: Tuple.Map[T, F] = p
  
  /* TODO: Determine if provide premade implicits for small (n < 22) tuple sizes has a positive effect on classfile size
  given [F[_], T1](using t1: F[T1]): ProductK[F, Tuple1[T1]] = Tuple1(t1)
  given [F[_], T1, T2](using t1: F[T1], t2: F[T2]): ProductK[F, (T1, T2)] = (t1, t2)
  given [F[_], T1, T2, T3](using t1: F[T1], t2: F[T2], t3: F[T3]): ProductK[F, (T1, T2, T3)] = (t1, t2, t3)
  given [F[_], T1, T2, T3, T4](using t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4]): ProductK[F, (T1, T2, T3, T4)] = (t1, t2, t3, t4)
   */
  
  given productKInstance[T <: Tuple](
    using size: ValueOf[Tuple.Size[T]]
  ): RepresentableKC[ProductKPar[T]] with TraverseKC[ProductKPar[T]] with
    type RepresentationK[_] = Finite[Tuple.Size[T]]
    
    extension[A[_], C](fa: ProductK[A, T]) 
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
          } else
            Applicative[G].map(acc)(a => Tuple.fromArray(a.reverseIterator.toArray).asInstanceOf[ProductK[B, T]])
  
        inner(Applicative[G].pure(List.empty[B[Any]]))

      override def indexK: RepresentationK ~>: A =
        [Z] => (rep: RepresentationK[Z]) => fa.productElement(rep.value).asInstanceOf[A[Z]]

    def tabulateK[A[_], C](f: RepresentationK ~>: A): ProductK[A, T] =
      //If the given is used, we already know that size > 0
      given (Finite.NotZero[Tuple.Size[T]] =:= true) = <:<.refl[Boolean].asInstanceOf[Finite.NotZero[Tuple.Size[T]] =:= true]
      val arr = IArray.tabulate(size.value)(i => f(Finite(size.value, i)))
      Tuple.fromIArray(arr).asInstanceOf[ProductK[A, T]]

trait ProductKGather:
  self: ProductK.type =>
  inline given gatherImplicits[F[_], T <: Tuple]: ProductK[F, T] = self.of(summonAll[Tuple.Map[T, F]].asInstanceOf[Tuple.Map[T, F]])
