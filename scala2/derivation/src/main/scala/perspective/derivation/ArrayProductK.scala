package perspective.derivation

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

import cats.{Applicative, Id}
import perspective.{Compose2, Const, Finite, RepresentableKC, TraverseKC, ~>#:, ~>:}

case class ArrayProductK[F[_], N <: Int with Singleton](arr: ArraySeq[F[_]])
object ArrayProductK {
  case class Foo(a: Int, b: String)

  new HKDProductGeneric[Foo] {
    trait Base
    trait Tag extends Any
    type G[F[_], T1, T2] <: Base with Tag
    private def makeG[F[_]](product: ArrayProductK[F, 2]): Gen[F] = product.asInstanceOf[Gen[F]]
    private def fromG[F[_]](product: Gen[F]): ArrayProductK[F, 2] = product.asInstanceOf[ArrayProductK[F, 2]]
    private val instance                                          = ArrayProductK.instance[2]

    override type Gen[F[_]] = G[F, Int, String]
    override def names: G[Const[String, *], Int, String] = makeG(ArrayProductK[Const[String, *], 2](ArraySeq("a", "b")))
    override def to(a: Foo): G[Id, Int, String]          = makeG(ArrayProductK[Id, 2](ArraySeq(a.a, a.b)))
    override def from(gen: G[Id, Int, String]): Foo = {
      val prod = fromG[Id](gen).arr
      Foo(prod(1).asInstanceOf[Int], prod(2).asInstanceOf[String])
    }

    override def representable: RepresentableKC[Gen] = instance.asInstanceOf[RepresentableKC[Gen]]
    override def traverse: TraverseKC[Gen]           = instance.asInstanceOf[TraverseKC[Gen]]
  }

  def instance[N <: Int with Singleton](
      implicit n: ValueOf[N]
  ): RepresentableKC.Aux[ArrayProductK[*[_], N], Const[Finite[N], *]] with TraverseKC[ArrayProductK[*[_], N]] =
    new RepresentableKC[ArrayProductK[*[_], N]] with TraverseKC[ArrayProductK[*[_], N]] {
      override type RepresentationK[A] = Const[Finite[N], A]

      override def indexK[A[_], C](fa: ArrayProductK[A, N]): RepresentationK ~>: A =
        new (RepresentationK ~>: A) {
          override def apply[Z](rep: RepresentationK[Z]): A[Z] = fa.arr(rep.value).asInstanceOf[A[Z]]
        }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): ArrayProductK[A, N] =
        ArrayProductK[A, N](
          ArraySeq.tabulate[Any](n.value)(m => f(Finite(n.value, m))).asInstanceOf[ArraySeq[A[_]]]
        )

      //TODO: Check correct
      override def traverseK[G[_]: Applicative, A[_], B[_], C](fa: ArrayProductK[A, N])(
          f: A ~>: Compose2[G, B, *]
      ): G[ArrayProductK[B, N]] = {
        @tailrec
        def inner(i: Int, acc: G[ArraySeq[B[Any]]]): G[ArrayProductK[B, N]] =
          if (fa.arr.isDefinedAt(i))
            inner(i + 1, Applicative[G].map2(f(fa.arr(i)), acc)((v, a) => a.updated(i, v.asInstanceOf[B[Any]])))
          else
            Applicative[G].map(acc)(a => ArrayProductK(a))

        inner(0, Applicative[G].pure(ArraySeq.empty[Any].asInstanceOf[ArraySeq[B[Any]]]))
      }

      override def foldLeftK[A[_], B, C](fa: ArrayProductK[A, N], b: B)(f: B => A ~>#: B): B =
        fa.arr.foldLeft(b)((b, a) => f(b)(a))
    }
}
