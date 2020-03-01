package perspective.derivation

import cats.Applicative
import perspective.{Compose2, Const, Finite, RepresentableKC, TraverseKC, ~>#:, ~>:}

case class Product1K[F[_], T1](p1: F[T1])
object Product1K {
  implicit def findInstances[F[_], T1](
      implicit p1: F[T1]
  ): Product1K[F, T1] = Product1K[F, T1](p1)

  implicit def product1KRepresentableTraverseInstance[T1]
      : RepresentableKC.Aux[Product1K[*[_], T1], Const[Finite[1], *]] with TraverseKC[Product1K[*[_], T1]] =
    new RepresentableKC[Product1K[*[_], T1]] with TraverseKC[Product1K[*[_], T1]] {
      override type RepresentationK[A] = Finite[1]

      override def indexK[A[_], C](fa: Product1K[A, T1]): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0 => fa.p1.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product1K[A, T1] =
        Product1K[A, T1](
          f(Finite(1, 0))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product1K[A, T1]
      )(f: A ~>: Compose2[G, B, *]): G[Product1K[B, T1]] =
        Applicative[G].map(f(fa.p1)) { (p1) =>
          Product1K[B, T1](p1)
        }

      override def foldLeftK[A[_], B, C](fa: Product1K[A, T1], b: B)(f: B => A ~>#: B): B =
        f(b)(fa.p1)
    }
}
case class Product2K[F[_], T1, T2](p1: F[T1], p2: F[T2])
object Product2K {
  implicit def findInstances[F[_], T1, T2](
      implicit p1: F[T1],
      p2: F[T2]
  ): Product2K[F, T1, T2] = Product2K[F, T1, T2](p1, p2)

  implicit def product2KRepresentableTraverseInstance[T1, T2]
      : RepresentableKC.Aux[Product2K[*[_], T1, T2], Const[Finite[2], *]] with TraverseKC[Product2K[*[_], T1, T2]] =
    new RepresentableKC[Product2K[*[_], T1, T2]] with TraverseKC[Product2K[*[_], T1, T2]] {
      override type RepresentationK[A] = Finite[2]

      override def indexK[A[_], C](fa: Product2K[A, T1, T2]): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0 => fa.p1.asInstanceOf[A[Z]]
          case 1 => fa.p2.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product2K[A, T1, T2] =
        Product2K[A, T1, T2](
          f(Finite(2, 0)),
          f(Finite(2, 1))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product2K[A, T1, T2]
      )(f: A ~>: Compose2[G, B, *]): G[Product2K[B, T1, T2]] =
        Applicative[G].map2(f(fa.p1), f(fa.p2)) { (p1, p2) =>
          Product2K[B, T1, T2](p1, p2)
        }

      override def foldLeftK[A[_], B, C](fa: Product2K[A, T1, T2], b: B)(f: B => A ~>#: B): B =
        f(f(b)(fa.p1))(fa.p2)
    }
}
case class Product3K[F[_], T1, T2, T3](p1: F[T1], p2: F[T2], p3: F[T3])
object Product3K {
  implicit def findInstances[F[_], T1, T2, T3](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3]
  ): Product3K[F, T1, T2, T3] = Product3K[F, T1, T2, T3](p1, p2, p3)

  implicit def product3KRepresentableTraverseInstance[T1, T2, T3]
      : RepresentableKC.Aux[Product3K[*[_], T1, T2, T3], Const[Finite[3], *]]
        with TraverseKC[Product3K[*[_], T1, T2, T3]] =
    new RepresentableKC[Product3K[*[_], T1, T2, T3]] with TraverseKC[Product3K[*[_], T1, T2, T3]] {
      override type RepresentationK[A] = Finite[3]

      override def indexK[A[_], C](fa: Product3K[A, T1, T2, T3]): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0 => fa.p1.asInstanceOf[A[Z]]
          case 1 => fa.p2.asInstanceOf[A[Z]]
          case 2 => fa.p3.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product3K[A, T1, T2, T3] =
        Product3K[A, T1, T2, T3](
          f(Finite(3, 0)),
          f(Finite(3, 1)),
          f(Finite(3, 2))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product3K[A, T1, T2, T3]
      )(f: A ~>: Compose2[G, B, *]): G[Product3K[B, T1, T2, T3]] =
        Applicative[G].map3(f(fa.p1), f(fa.p2), f(fa.p3)) { (p1, p2, p3) =>
          Product3K[B, T1, T2, T3](p1, p2, p3)
        }

      override def foldLeftK[A[_], B, C](fa: Product3K[A, T1, T2, T3], b: B)(f: B => A ~>#: B): B =
        f(f(f(b)(fa.p1))(fa.p2))(fa.p3)
    }
}
case class Product4K[F[_], T1, T2, T3, T4](p1: F[T1], p2: F[T2], p3: F[T3], p4: F[T4])
object Product4K {
  implicit def findInstances[F[_], T1, T2, T3, T4](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4]
  ): Product4K[F, T1, T2, T3, T4] = Product4K[F, T1, T2, T3, T4](p1, p2, p3, p4)

  implicit def product4KRepresentableTraverseInstance[T1, T2, T3, T4]
      : RepresentableKC.Aux[Product4K[*[_], T1, T2, T3, T4], Const[Finite[4], *]]
        with TraverseKC[Product4K[*[_], T1, T2, T3, T4]] =
    new RepresentableKC[Product4K[*[_], T1, T2, T3, T4]] with TraverseKC[Product4K[*[_], T1, T2, T3, T4]] {
      override type RepresentationK[A] = Finite[4]

      override def indexK[A[_], C](fa: Product4K[A, T1, T2, T3, T4]): RepresentationK ~>: A =
        new (RepresentationK ~>: A) {
          override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
            case 0 => fa.p1.asInstanceOf[A[Z]]
            case 1 => fa.p2.asInstanceOf[A[Z]]
            case 2 => fa.p3.asInstanceOf[A[Z]]
            case 3 => fa.p4.asInstanceOf[A[Z]]
          }
        }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product4K[A, T1, T2, T3, T4] =
        Product4K[A, T1, T2, T3, T4](
          f(Finite(4, 0)),
          f(Finite(4, 1)),
          f(Finite(4, 2)),
          f(Finite(4, 3))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product4K[A, T1, T2, T3, T4]
      )(f: A ~>: Compose2[G, B, *]): G[Product4K[B, T1, T2, T3, T4]] =
        Applicative[G].map4(f(fa.p1), f(fa.p2), f(fa.p3), f(fa.p4)) { (p1, p2, p3, p4) =>
          Product4K[B, T1, T2, T3, T4](p1, p2, p3, p4)
        }

      override def foldLeftK[A[_], B, C](fa: Product4K[A, T1, T2, T3, T4], b: B)(f: B => A ~>#: B): B =
        f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4)
    }
}
case class Product5K[F[_], T1, T2, T3, T4, T5](p1: F[T1], p2: F[T2], p3: F[T3], p4: F[T4], p5: F[T5])
object Product5K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5]
  ): Product5K[F, T1, T2, T3, T4, T5] = Product5K[F, T1, T2, T3, T4, T5](p1, p2, p3, p4, p5)

  implicit def product5KRepresentableTraverseInstance[T1, T2, T3, T4, T5]
      : RepresentableKC.Aux[Product5K[*[_], T1, T2, T3, T4, T5], Const[Finite[5], *]]
        with TraverseKC[Product5K[*[_], T1, T2, T3, T4, T5]] =
    new RepresentableKC[Product5K[*[_], T1, T2, T3, T4, T5]] with TraverseKC[Product5K[*[_], T1, T2, T3, T4, T5]] {
      override type RepresentationK[A] = Finite[5]

      override def indexK[A[_], C](fa: Product5K[A, T1, T2, T3, T4, T5]): RepresentationK ~>: A =
        new (RepresentationK ~>: A) {
          override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
            case 0 => fa.p1.asInstanceOf[A[Z]]
            case 1 => fa.p2.asInstanceOf[A[Z]]
            case 2 => fa.p3.asInstanceOf[A[Z]]
            case 3 => fa.p4.asInstanceOf[A[Z]]
            case 4 => fa.p5.asInstanceOf[A[Z]]
          }
        }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product5K[A, T1, T2, T3, T4, T5] =
        Product5K[A, T1, T2, T3, T4, T5](
          f(Finite(5, 0)),
          f(Finite(5, 1)),
          f(Finite(5, 2)),
          f(Finite(5, 3)),
          f(Finite(5, 4))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product5K[A, T1, T2, T3, T4, T5]
      )(f: A ~>: Compose2[G, B, *]): G[Product5K[B, T1, T2, T3, T4, T5]] =
        Applicative[G].map5(f(fa.p1), f(fa.p2), f(fa.p3), f(fa.p4), f(fa.p5)) { (p1, p2, p3, p4, p5) =>
          Product5K[B, T1, T2, T3, T4, T5](p1, p2, p3, p4, p5)
        }

      override def foldLeftK[A[_], B, C](fa: Product5K[A, T1, T2, T3, T4, T5], b: B)(f: B => A ~>#: B): B =
        f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5)
    }
}
case class Product6K[F[_], T1, T2, T3, T4, T5, T6](p1: F[T1], p2: F[T2], p3: F[T3], p4: F[T4], p5: F[T5], p6: F[T6])
object Product6K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6]
  ): Product6K[F, T1, T2, T3, T4, T5, T6] = Product6K[F, T1, T2, T3, T4, T5, T6](p1, p2, p3, p4, p5, p6)

  implicit def product6KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6]
      : RepresentableKC.Aux[Product6K[*[_], T1, T2, T3, T4, T5, T6], Const[Finite[6], *]]
        with TraverseKC[Product6K[*[_], T1, T2, T3, T4, T5, T6]] =
    new RepresentableKC[Product6K[*[_], T1, T2, T3, T4, T5, T6]]
    with TraverseKC[Product6K[*[_], T1, T2, T3, T4, T5, T6]] {
      override type RepresentationK[A] = Finite[6]

      override def indexK[A[_], C](fa: Product6K[A, T1, T2, T3, T4, T5, T6]): RepresentationK ~>: A =
        new (RepresentationK ~>: A) {
          override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
            case 0 => fa.p1.asInstanceOf[A[Z]]
            case 1 => fa.p2.asInstanceOf[A[Z]]
            case 2 => fa.p3.asInstanceOf[A[Z]]
            case 3 => fa.p4.asInstanceOf[A[Z]]
            case 4 => fa.p5.asInstanceOf[A[Z]]
            case 5 => fa.p6.asInstanceOf[A[Z]]
          }
        }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product6K[A, T1, T2, T3, T4, T5, T6] =
        Product6K[A, T1, T2, T3, T4, T5, T6](
          f(Finite(6, 0)),
          f(Finite(6, 1)),
          f(Finite(6, 2)),
          f(Finite(6, 3)),
          f(Finite(6, 4)),
          f(Finite(6, 5))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product6K[A, T1, T2, T3, T4, T5, T6]
      )(f: A ~>: Compose2[G, B, *]): G[Product6K[B, T1, T2, T3, T4, T5, T6]] =
        Applicative[G].map6(f(fa.p1), f(fa.p2), f(fa.p3), f(fa.p4), f(fa.p5), f(fa.p6)) { (p1, p2, p3, p4, p5, p6) =>
          Product6K[B, T1, T2, T3, T4, T5, T6](p1, p2, p3, p4, p5, p6)
        }

      override def foldLeftK[A[_], B, C](fa: Product6K[A, T1, T2, T3, T4, T5, T6], b: B)(f: B => A ~>#: B): B =
        f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6)
    }
}
case class Product7K[F[_], T1, T2, T3, T4, T5, T6, T7](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7]
)
object Product7K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7]
  ): Product7K[F, T1, T2, T3, T4, T5, T6, T7] = Product7K[F, T1, T2, T3, T4, T5, T6, T7](p1, p2, p3, p4, p5, p6, p7)

  implicit def product7KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7]
      : RepresentableKC.Aux[Product7K[*[_], T1, T2, T3, T4, T5, T6, T7], Const[Finite[7], *]]
        with TraverseKC[Product7K[*[_], T1, T2, T3, T4, T5, T6, T7]] =
    new RepresentableKC[Product7K[*[_], T1, T2, T3, T4, T5, T6, T7]]
    with TraverseKC[Product7K[*[_], T1, T2, T3, T4, T5, T6, T7]] {
      override type RepresentationK[A] = Finite[7]

      override def indexK[A[_], C](fa: Product7K[A, T1, T2, T3, T4, T5, T6, T7]): RepresentationK ~>: A =
        new (RepresentationK ~>: A) {
          override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
            case 0 => fa.p1.asInstanceOf[A[Z]]
            case 1 => fa.p2.asInstanceOf[A[Z]]
            case 2 => fa.p3.asInstanceOf[A[Z]]
            case 3 => fa.p4.asInstanceOf[A[Z]]
            case 4 => fa.p5.asInstanceOf[A[Z]]
            case 5 => fa.p6.asInstanceOf[A[Z]]
            case 6 => fa.p7.asInstanceOf[A[Z]]
          }
        }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product7K[A, T1, T2, T3, T4, T5, T6, T7] =
        Product7K[A, T1, T2, T3, T4, T5, T6, T7](
          f(Finite(7, 0)),
          f(Finite(7, 1)),
          f(Finite(7, 2)),
          f(Finite(7, 3)),
          f(Finite(7, 4)),
          f(Finite(7, 5)),
          f(Finite(7, 6))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product7K[A, T1, T2, T3, T4, T5, T6, T7]
      )(f: A ~>: Compose2[G, B, *]): G[Product7K[B, T1, T2, T3, T4, T5, T6, T7]] =
        Applicative[G].map7(f(fa.p1), f(fa.p2), f(fa.p3), f(fa.p4), f(fa.p5), f(fa.p6), f(fa.p7)) {
          (p1, p2, p3, p4, p5, p6, p7) =>
            Product7K[B, T1, T2, T3, T4, T5, T6, T7](p1, p2, p3, p4, p5, p6, p7)
        }

      override def foldLeftK[A[_], B, C](fa: Product7K[A, T1, T2, T3, T4, T5, T6, T7], b: B)(f: B => A ~>#: B): B =
        f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7)
    }
}
case class Product8K[F[_], T1, T2, T3, T4, T5, T6, T7, T8](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8]
)
object Product8K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8]
  ): Product8K[F, T1, T2, T3, T4, T5, T6, T7, T8] =
    Product8K[F, T1, T2, T3, T4, T5, T6, T7, T8](p1, p2, p3, p4, p5, p6, p7, p8)

  implicit def product8KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7, T8]
      : RepresentableKC.Aux[Product8K[*[_], T1, T2, T3, T4, T5, T6, T7, T8], Const[Finite[8], *]]
        with TraverseKC[Product8K[*[_], T1, T2, T3, T4, T5, T6, T7, T8]] =
    new RepresentableKC[Product8K[*[_], T1, T2, T3, T4, T5, T6, T7, T8]]
    with TraverseKC[Product8K[*[_], T1, T2, T3, T4, T5, T6, T7, T8]] {
      override type RepresentationK[A] = Finite[8]

      override def indexK[A[_], C](fa: Product8K[A, T1, T2, T3, T4, T5, T6, T7, T8]): RepresentationK ~>: A =
        new (RepresentationK ~>: A) {
          override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
            case 0 => fa.p1.asInstanceOf[A[Z]]
            case 1 => fa.p2.asInstanceOf[A[Z]]
            case 2 => fa.p3.asInstanceOf[A[Z]]
            case 3 => fa.p4.asInstanceOf[A[Z]]
            case 4 => fa.p5.asInstanceOf[A[Z]]
            case 5 => fa.p6.asInstanceOf[A[Z]]
            case 6 => fa.p7.asInstanceOf[A[Z]]
            case 7 => fa.p8.asInstanceOf[A[Z]]
          }
        }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product8K[A, T1, T2, T3, T4, T5, T6, T7, T8] =
        Product8K[A, T1, T2, T3, T4, T5, T6, T7, T8](
          f(Finite(8, 0)),
          f(Finite(8, 1)),
          f(Finite(8, 2)),
          f(Finite(8, 3)),
          f(Finite(8, 4)),
          f(Finite(8, 5)),
          f(Finite(8, 6)),
          f(Finite(8, 7))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product8K[A, T1, T2, T3, T4, T5, T6, T7, T8]
      )(f: A ~>: Compose2[G, B, *]): G[Product8K[B, T1, T2, T3, T4, T5, T6, T7, T8]] =
        Applicative[G].map8(f(fa.p1), f(fa.p2), f(fa.p3), f(fa.p4), f(fa.p5), f(fa.p6), f(fa.p7), f(fa.p8)) {
          (p1, p2, p3, p4, p5, p6, p7, p8) =>
            Product8K[B, T1, T2, T3, T4, T5, T6, T7, T8](p1, p2, p3, p4, p5, p6, p7, p8)
        }

      override def foldLeftK[A[_], B, C](fa: Product8K[A, T1, T2, T3, T4, T5, T6, T7, T8], b: B)(f: B => A ~>#: B): B =
        f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8)
    }
}
case class Product9K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9]
)
object Product9K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9]
  ): Product9K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Product9K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9](p1, p2, p3, p4, p5, p6, p7, p8, p9)

  implicit def product9KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7, T8, T9]
      : RepresentableKC.Aux[Product9K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9], Const[Finite[9], *]]
        with TraverseKC[Product9K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
    new RepresentableKC[Product9K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9]]
    with TraverseKC[Product9K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9]] {
      override type RepresentationK[A] = Finite[9]

      override def indexK[A[_], C](fa: Product9K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9]): RepresentationK ~>: A =
        new (RepresentationK ~>: A) {
          override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
            case 0 => fa.p1.asInstanceOf[A[Z]]
            case 1 => fa.p2.asInstanceOf[A[Z]]
            case 2 => fa.p3.asInstanceOf[A[Z]]
            case 3 => fa.p4.asInstanceOf[A[Z]]
            case 4 => fa.p5.asInstanceOf[A[Z]]
            case 5 => fa.p6.asInstanceOf[A[Z]]
            case 6 => fa.p7.asInstanceOf[A[Z]]
            case 7 => fa.p8.asInstanceOf[A[Z]]
            case 8 => fa.p9.asInstanceOf[A[Z]]
          }
        }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product9K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9] =
        Product9K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9](
          f(Finite(9, 0)),
          f(Finite(9, 1)),
          f(Finite(9, 2)),
          f(Finite(9, 3)),
          f(Finite(9, 4)),
          f(Finite(9, 5)),
          f(Finite(9, 6)),
          f(Finite(9, 7)),
          f(Finite(9, 8))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product9K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9]
      )(f: A ~>: Compose2[G, B, *]): G[Product9K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
        Applicative[G].map9(f(fa.p1), f(fa.p2), f(fa.p3), f(fa.p4), f(fa.p5), f(fa.p6), f(fa.p7), f(fa.p8), f(fa.p9)) {
          (p1, p2, p3, p4, p5, p6, p7, p8, p9) =>
            Product9K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9](p1, p2, p3, p4, p5, p6, p7, p8, p9)
        }

      override def foldLeftK[A[_], B, C](fa: Product9K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9], b: B)(
          f: B => A ~>#: B
      ): B =
        f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9)
    }
}
case class Product10K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10]
)
object Product10K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10]
  ): Product10K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Product10K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

  implicit def product10KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
      : RepresentableKC.Aux[Product10K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], Const[Finite[10], *]]
        with TraverseKC[Product10K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
    new RepresentableKC[Product10K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]
    with TraverseKC[Product10K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] {
      override type RepresentationK[A] = Finite[10]

      override def indexK[A[_], C](fa: Product10K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): RepresentationK ~>: A =
        new (RepresentationK ~>: A) {
          override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
            case 0 => fa.p1.asInstanceOf[A[Z]]
            case 1 => fa.p2.asInstanceOf[A[Z]]
            case 2 => fa.p3.asInstanceOf[A[Z]]
            case 3 => fa.p4.asInstanceOf[A[Z]]
            case 4 => fa.p5.asInstanceOf[A[Z]]
            case 5 => fa.p6.asInstanceOf[A[Z]]
            case 6 => fa.p7.asInstanceOf[A[Z]]
            case 7 => fa.p8.asInstanceOf[A[Z]]
            case 8 => fa.p9.asInstanceOf[A[Z]]
            case 9 => fa.p10.asInstanceOf[A[Z]]
          }
        }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product10K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
        Product10K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
          f(Finite(10, 0)),
          f(Finite(10, 1)),
          f(Finite(10, 2)),
          f(Finite(10, 3)),
          f(Finite(10, 4)),
          f(Finite(10, 5)),
          f(Finite(10, 6)),
          f(Finite(10, 7)),
          f(Finite(10, 8)),
          f(Finite(10, 9))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product10K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
      )(f: A ~>: Compose2[G, B, *]): G[Product10K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
        Applicative[G].map10(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) =>
          Product10K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
        }

      override def foldLeftK[A[_], B, C](fa: Product10K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], b: B)(
          f: B => A ~>#: B
      ): B =
        f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(fa.p10)
    }
}
case class Product11K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11]
)
object Product11K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11]
  ): Product11K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] =
    Product11K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)

  implicit def product11KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
      : RepresentableKC.Aux[Product11K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], Const[Finite[11], *]]
        with TraverseKC[Product11K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] =
    new RepresentableKC[Product11K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]
    with TraverseKC[Product11K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] {
      override type RepresentationK[A] = Finite[11]

      override def indexK[A[_], C](
          fa: Product11K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product11K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] =
        Product11K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
          f(Finite(11, 0)),
          f(Finite(11, 1)),
          f(Finite(11, 2)),
          f(Finite(11, 3)),
          f(Finite(11, 4)),
          f(Finite(11, 5)),
          f(Finite(11, 6)),
          f(Finite(11, 7)),
          f(Finite(11, 8)),
          f(Finite(11, 9)),
          f(Finite(11, 10))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product11K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
      )(f: A ~>: Compose2[G, B, *]): G[Product11K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] =
        Applicative[G].map11(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) =>
          Product11K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
        }

      override def foldLeftK[A[_], B, C](fa: Product11K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], b: B)(
          f: B => A ~>#: B
      ): B =
        f(f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(fa.p10))(
          fa.p11
        )
    }
}
case class Product12K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12]
)
object Product12K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12]
  ): Product12K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] =
    Product12K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)

  implicit def product12KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
      : RepresentableKC.Aux[Product12K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], Const[Finite[12], *]]
        with TraverseKC[Product12K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] =
    new RepresentableKC[Product12K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]
    with TraverseKC[Product12K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] {
      override type RepresentationK[A] = Finite[12]

      override def indexK[A[_], C](
          fa: Product12K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product12K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] =
        Product12K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
          f(Finite(12, 0)),
          f(Finite(12, 1)),
          f(Finite(12, 2)),
          f(Finite(12, 3)),
          f(Finite(12, 4)),
          f(Finite(12, 5)),
          f(Finite(12, 6)),
          f(Finite(12, 7)),
          f(Finite(12, 8)),
          f(Finite(12, 9)),
          f(Finite(12, 10)),
          f(Finite(12, 11))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product12K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
      )(f: A ~>: Compose2[G, B, *]): G[Product12K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] =
        Applicative[G].map12(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) =>
          Product12K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12
          )
        }

      override def foldLeftK[A[_], B, C](fa: Product12K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], b: B)(
          f: B => A ~>#: B
      ): B =
        f(
          f(f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(fa.p10))(
            fa.p11
          )
        )(fa.p12)
    }
}
case class Product13K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13]
)
object Product13K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13]
  ): Product13K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] =
    Product13K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13
    )

  implicit def product13KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
      : RepresentableKC.Aux[Product13K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], Const[
        Finite[13],
        *
      ]] with TraverseKC[Product13K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] =
    new RepresentableKC[Product13K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]
    with TraverseKC[Product13K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] {
      override type RepresentationK[A] = Finite[13]

      override def indexK[A[_], C](
          fa: Product13K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product13K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] =
        Product13K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
          f(Finite(13, 0)),
          f(Finite(13, 1)),
          f(Finite(13, 2)),
          f(Finite(13, 3)),
          f(Finite(13, 4)),
          f(Finite(13, 5)),
          f(Finite(13, 6)),
          f(Finite(13, 7)),
          f(Finite(13, 8)),
          f(Finite(13, 9)),
          f(Finite(13, 10)),
          f(Finite(13, 11)),
          f(Finite(13, 12))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product13K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
      )(f: A ~>: Compose2[G, B, *]): G[Product13K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] =
        Applicative[G].map13(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) =>
          Product13K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product13K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(fa.p10))(
              fa.p11
            )
          )(fa.p12)
        )(fa.p13)
    }
}
case class Product14K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14]
)
object Product14K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14]
  ): Product14K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] =
    Product14K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14
    )

  implicit def product14KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
      : RepresentableKC.Aux[Product14K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], Const[Finite[
        14
      ], *]] with TraverseKC[Product14K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] =
    new RepresentableKC[Product14K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]
    with TraverseKC[Product14K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] {
      override type RepresentationK[A] = Finite[14]

      override def indexK[A[_], C](
          fa: Product14K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product14K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] =
        Product14K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
          f(Finite(14, 0)),
          f(Finite(14, 1)),
          f(Finite(14, 2)),
          f(Finite(14, 3)),
          f(Finite(14, 4)),
          f(Finite(14, 5)),
          f(Finite(14, 6)),
          f(Finite(14, 7)),
          f(Finite(14, 8)),
          f(Finite(14, 9)),
          f(Finite(14, 10)),
          f(Finite(14, 11)),
          f(Finite(14, 12)),
          f(Finite(14, 13))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product14K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
      )(f: A ~>: Compose2[G, B, *]): G[Product14K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] =
        Applicative[G].map14(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14) =>
          Product14K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13,
            p14
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product14K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(fa.p10))(
                fa.p11
              )
            )(fa.p12)
          )(fa.p13)
        )(fa.p14)
    }
}
case class Product15K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14],
    p15: F[T15]
)
object Product15K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14],
      p15: F[T15]
  ): Product15K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] =
    Product15K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15
    )

  implicit def product15KRepresentableTraverseInstance[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
      : RepresentableKC.Aux[Product15K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], Const[
        Finite[15],
        *
      ]] with TraverseKC[Product15K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] =
    new RepresentableKC[Product15K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]
    with TraverseKC[Product15K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] {
      override type RepresentationK[A] = Finite[15]

      override def indexK[A[_], C](
          fa: Product15K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
          case 14 => fa.p15.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product15K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] =
        Product15K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
          f(Finite(15, 0)),
          f(Finite(15, 1)),
          f(Finite(15, 2)),
          f(Finite(15, 3)),
          f(Finite(15, 4)),
          f(Finite(15, 5)),
          f(Finite(15, 6)),
          f(Finite(15, 7)),
          f(Finite(15, 8)),
          f(Finite(15, 9)),
          f(Finite(15, 10)),
          f(Finite(15, 11)),
          f(Finite(15, 12)),
          f(Finite(15, 13)),
          f(Finite(15, 14))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product15K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
      )(
          f: A ~>: Compose2[G, B, *]
      ): G[Product15K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] =
        Applicative[G].map15(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14),
          f(fa.p15)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15) =>
          Product15K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13,
            p14,
            p15
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product15K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(fa.p10)
                )(fa.p11)
              )(fa.p12)
            )(fa.p13)
          )(fa.p14)
        )(fa.p15)
    }
}
case class Product16K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14],
    p15: F[T15],
    p16: F[T16]
)
object Product16K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14],
      p15: F[T15],
      p16: F[T16]
  ): Product16K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] =
    Product16K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15,
      p16
    )

  implicit def product16KRepresentableTraverseInstance[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16
  ]: RepresentableKC.Aux[Product16K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], Const[
    Finite[16],
    *
  ]] with TraverseKC[Product16K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] =
    new RepresentableKC[Product16K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]
    with TraverseKC[Product16K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] {
      override type RepresentationK[A] = Finite[16]

      override def indexK[A[_], C](
          fa: Product16K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
          case 14 => fa.p15.asInstanceOf[A[Z]]
          case 15 => fa.p16.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product16K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] =
        Product16K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
          f(Finite(16, 0)),
          f(Finite(16, 1)),
          f(Finite(16, 2)),
          f(Finite(16, 3)),
          f(Finite(16, 4)),
          f(Finite(16, 5)),
          f(Finite(16, 6)),
          f(Finite(16, 7)),
          f(Finite(16, 8)),
          f(Finite(16, 9)),
          f(Finite(16, 10)),
          f(Finite(16, 11)),
          f(Finite(16, 12)),
          f(Finite(16, 13)),
          f(Finite(16, 14)),
          f(Finite(16, 15))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product16K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
      )(
          f: A ~>: Compose2[G, B, *]
      ): G[Product16K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] =
        Applicative[G].map16(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14),
          f(fa.p15),
          f(fa.p16)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) =>
          Product16K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13,
            p14,
            p15,
            p16
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product16K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(
                      fa.p10
                    )
                  )(fa.p11)
                )(fa.p12)
              )(fa.p13)
            )(fa.p14)
          )(fa.p15)
        )(fa.p16)
    }
}
case class Product17K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14],
    p15: F[T15],
    p16: F[T16],
    p17: F[T17]
)
object Product17K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14],
      p15: F[T15],
      p16: F[T16],
      p17: F[T17]
  ): Product17K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] =
    Product17K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15,
      p16,
      p17
    )

  implicit def product17KRepresentableTraverseInstance[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17
  ]: RepresentableKC.Aux[Product17K[
    *[_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17
  ], Const[Finite[17], *]]
    with TraverseKC[Product17K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] =
    new RepresentableKC[Product17K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]
    with TraverseKC[Product17K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] {
      override type RepresentationK[A] = Finite[17]

      override def indexK[A[_], C](
          fa: Product17K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
          case 14 => fa.p15.asInstanceOf[A[Z]]
          case 15 => fa.p16.asInstanceOf[A[Z]]
          case 16 => fa.p17.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product17K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] =
        Product17K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
          f(Finite(17, 0)),
          f(Finite(17, 1)),
          f(Finite(17, 2)),
          f(Finite(17, 3)),
          f(Finite(17, 4)),
          f(Finite(17, 5)),
          f(Finite(17, 6)),
          f(Finite(17, 7)),
          f(Finite(17, 8)),
          f(Finite(17, 9)),
          f(Finite(17, 10)),
          f(Finite(17, 11)),
          f(Finite(17, 12)),
          f(Finite(17, 13)),
          f(Finite(17, 14)),
          f(Finite(17, 15)),
          f(Finite(17, 16))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product17K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
      )(
          f: A ~>: Compose2[G, B, *]
      ): G[Product17K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] =
        Applicative[G].map17(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14),
          f(fa.p15),
          f(fa.p16),
          f(fa.p17)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17) =>
          Product17K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13,
            p14,
            p15,
            p16,
            p17
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product17K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(
                        fa.p10
                      )
                    )(fa.p11)
                  )(fa.p12)
                )(fa.p13)
              )(fa.p14)
            )(fa.p15)
          )(fa.p16)
        )(fa.p17)
    }
}
case class Product18K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14],
    p15: F[T15],
    p16: F[T16],
    p17: F[T17],
    p18: F[T18]
)
object Product18K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14],
      p15: F[T15],
      p16: F[T16],
      p17: F[T17],
      p18: F[T18]
  ): Product18K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] =
    Product18K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15,
      p16,
      p17,
      p18
    )

  implicit def product18KRepresentableTraverseInstance[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18
  ]: RepresentableKC.Aux[Product18K[
    *[_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18
  ], Const[Finite[18], *]]
    with TraverseKC[Product18K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] =
    new RepresentableKC[
      Product18K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    ]
    with TraverseKC[Product18K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] {
      override type RepresentationK[A] = Finite[18]

      override def indexK[A[_], C](
          fa: Product18K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
          case 14 => fa.p15.asInstanceOf[A[Z]]
          case 15 => fa.p16.asInstanceOf[A[Z]]
          case 16 => fa.p17.asInstanceOf[A[Z]]
          case 17 => fa.p18.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product18K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] =
        Product18K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
          f(Finite(18, 0)),
          f(Finite(18, 1)),
          f(Finite(18, 2)),
          f(Finite(18, 3)),
          f(Finite(18, 4)),
          f(Finite(18, 5)),
          f(Finite(18, 6)),
          f(Finite(18, 7)),
          f(Finite(18, 8)),
          f(Finite(18, 9)),
          f(Finite(18, 10)),
          f(Finite(18, 11)),
          f(Finite(18, 12)),
          f(Finite(18, 13)),
          f(Finite(18, 14)),
          f(Finite(18, 15)),
          f(Finite(18, 16)),
          f(Finite(18, 17))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product18K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
      )(
          f: A ~>: Compose2[G, B, *]
      ): G[Product18K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] =
        Applicative[G].map18(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14),
          f(fa.p15),
          f(fa.p16),
          f(fa.p17),
          f(fa.p18)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18) =>
          Product18K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13,
            p14,
            p15,
            p16,
            p17,
            p18
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product18K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9))(
                          fa.p10
                        )
                      )(fa.p11)
                    )(fa.p12)
                  )(fa.p13)
                )(fa.p14)
              )(fa.p15)
            )(fa.p16)
          )(fa.p17)
        )(fa.p18)
    }
}
case class Product19K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14],
    p15: F[T15],
    p16: F[T16],
    p17: F[T17],
    p18: F[T18],
    p19: F[T19]
)
object Product19K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14],
      p15: F[T15],
      p16: F[T16],
      p17: F[T17],
      p18: F[T18],
      p19: F[T19]
  ): Product19K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] =
    Product19K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15,
      p16,
      p17,
      p18,
      p19
    )

  implicit def product19KRepresentableTraverseInstance[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19
  ]: RepresentableKC.Aux[Product19K[
    *[_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19
  ], Const[Finite[19], *]]
    with TraverseKC[
      Product19K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    ] =
    new RepresentableKC[
      Product19K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    ] with TraverseKC[
      Product19K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    ] {
      override type RepresentationK[A] = Finite[19]

      override def indexK[A[_], C](
          fa: Product19K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
          case 14 => fa.p15.asInstanceOf[A[Z]]
          case 15 => fa.p16.asInstanceOf[A[Z]]
          case 16 => fa.p17.asInstanceOf[A[Z]]
          case 17 => fa.p18.asInstanceOf[A[Z]]
          case 18 => fa.p19.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product19K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] =
        Product19K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
          f(Finite(19, 0)),
          f(Finite(19, 1)),
          f(Finite(19, 2)),
          f(Finite(19, 3)),
          f(Finite(19, 4)),
          f(Finite(19, 5)),
          f(Finite(19, 6)),
          f(Finite(19, 7)),
          f(Finite(19, 8)),
          f(Finite(19, 9)),
          f(Finite(19, 10)),
          f(Finite(19, 11)),
          f(Finite(19, 12)),
          f(Finite(19, 13)),
          f(Finite(19, 14)),
          f(Finite(19, 15)),
          f(Finite(19, 16)),
          f(Finite(19, 17)),
          f(Finite(19, 18))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product19K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
      )(
          f: A ~>: Compose2[G, B, *]
      ): G[Product19K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] =
        Applicative[G].map19(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14),
          f(fa.p15),
          f(fa.p16),
          f(fa.p17),
          f(fa.p18),
          f(fa.p19)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19) =>
          Product19K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13,
            p14,
            p15,
            p16,
            p17,
            p18,
            p19
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product19K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(
                          f(
                            f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(fa.p9)
                          )(fa.p10)
                        )(fa.p11)
                      )(fa.p12)
                    )(fa.p13)
                  )(fa.p14)
                )(fa.p15)
              )(fa.p16)
            )(fa.p17)
          )(fa.p18)
        )(fa.p19)
    }
}
case class Product20K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14],
    p15: F[T15],
    p16: F[T16],
    p17: F[T17],
    p18: F[T18],
    p19: F[T19],
    p20: F[T20]
)
object Product20K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14],
      p15: F[T15],
      p16: F[T16],
      p17: F[T17],
      p18: F[T18],
      p19: F[T19],
      p20: F[T20]
  ): Product20K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] =
    Product20K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15,
      p16,
      p17,
      p18,
      p19,
      p20
    )

  implicit def product20KRepresentableTraverseInstance[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20
  ]: RepresentableKC.Aux[Product20K[
    *[_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20
  ], Const[Finite[20], *]]
    with TraverseKC[
      Product20K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    ] =
    new RepresentableKC[
      Product20K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    ] with TraverseKC[
      Product20K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    ] {
      override type RepresentationK[A] = Finite[20]

      override def indexK[A[_], C](
          fa: Product20K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
          case 14 => fa.p15.asInstanceOf[A[Z]]
          case 15 => fa.p16.asInstanceOf[A[Z]]
          case 16 => fa.p17.asInstanceOf[A[Z]]
          case 17 => fa.p18.asInstanceOf[A[Z]]
          case 18 => fa.p19.asInstanceOf[A[Z]]
          case 19 => fa.p20.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product20K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] =
        Product20K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
          f(Finite(20, 0)),
          f(Finite(20, 1)),
          f(Finite(20, 2)),
          f(Finite(20, 3)),
          f(Finite(20, 4)),
          f(Finite(20, 5)),
          f(Finite(20, 6)),
          f(Finite(20, 7)),
          f(Finite(20, 8)),
          f(Finite(20, 9)),
          f(Finite(20, 10)),
          f(Finite(20, 11)),
          f(Finite(20, 12)),
          f(Finite(20, 13)),
          f(Finite(20, 14)),
          f(Finite(20, 15)),
          f(Finite(20, 16)),
          f(Finite(20, 17)),
          f(Finite(20, 18)),
          f(Finite(20, 19))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product20K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
      )(
          f: A ~>: Compose2[G, B, *]
      ): G[Product20K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] =
        Applicative[G].map20(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14),
          f(fa.p15),
          f(fa.p16),
          f(fa.p17),
          f(fa.p18),
          f(fa.p19),
          f(fa.p20)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20) =>
          Product20K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13,
            p14,
            p15,
            p16,
            p17,
            p18,
            p19,
            p20
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product20K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(
                          f(
                            f(
                              f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(
                                fa.p9
                              )
                            )(fa.p10)
                          )(fa.p11)
                        )(fa.p12)
                      )(fa.p13)
                    )(fa.p14)
                  )(fa.p15)
                )(fa.p16)
              )(fa.p17)
            )(fa.p18)
          )(fa.p19)
        )(fa.p20)
    }
}
case class Product21K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14],
    p15: F[T15],
    p16: F[T16],
    p17: F[T17],
    p18: F[T18],
    p19: F[T19],
    p20: F[T20],
    p21: F[T21]
)
object Product21K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14],
      p15: F[T15],
      p16: F[T16],
      p17: F[T17],
      p18: F[T18],
      p19: F[T19],
      p20: F[T20],
      p21: F[T21]
  ): Product21K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] =
    Product21K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15,
      p16,
      p17,
      p18,
      p19,
      p20,
      p21
    )

  implicit def product21KRepresentableTraverseInstance[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21
  ]: RepresentableKC.Aux[Product21K[
    *[_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21
  ], Const[Finite[21], *]]
    with TraverseKC[
      Product21K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
    ] =
    new RepresentableKC[
      Product21K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
    ] with TraverseKC[
      Product21K[*[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
    ] {
      override type RepresentationK[A] = Finite[21]

      override def indexK[A[_], C](
          fa: Product21K[
            A,
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21
          ]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
          case 14 => fa.p15.asInstanceOf[A[Z]]
          case 15 => fa.p16.asInstanceOf[A[Z]]
          case 16 => fa.p17.asInstanceOf[A[Z]]
          case 17 => fa.p18.asInstanceOf[A[Z]]
          case 18 => fa.p19.asInstanceOf[A[Z]]
          case 19 => fa.p20.asInstanceOf[A[Z]]
          case 20 => fa.p21.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](
          f: RepresentationK ~>: A
      ): Product21K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] =
        Product21K[A, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
          f(Finite(21, 0)),
          f(Finite(21, 1)),
          f(Finite(21, 2)),
          f(Finite(21, 3)),
          f(Finite(21, 4)),
          f(Finite(21, 5)),
          f(Finite(21, 6)),
          f(Finite(21, 7)),
          f(Finite(21, 8)),
          f(Finite(21, 9)),
          f(Finite(21, 10)),
          f(Finite(21, 11)),
          f(Finite(21, 12)),
          f(Finite(21, 13)),
          f(Finite(21, 14)),
          f(Finite(21, 15)),
          f(Finite(21, 16)),
          f(Finite(21, 17)),
          f(Finite(21, 18)),
          f(Finite(21, 19)),
          f(Finite(21, 20))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product21K[
            A,
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21
          ]
      )(f: A ~>: Compose2[G, B, *]): G[
        Product21K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
      ] =
        Applicative[G].map21(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14),
          f(fa.p15),
          f(fa.p16),
          f(fa.p17),
          f(fa.p18),
          f(fa.p19),
          f(fa.p20),
          f(fa.p21)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21) =>
          Product21K[B, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
            p1,
            p2,
            p3,
            p4,
            p5,
            p6,
            p7,
            p8,
            p9,
            p10,
            p11,
            p12,
            p13,
            p14,
            p15,
            p16,
            p17,
            p18,
            p19,
            p20,
            p21
          )
        }

      override def foldLeftK[A[_], B, C](
          fa: Product21K[
            A,
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21
          ],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(
                          f(
                            f(
                              f(
                                f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(
                                  fa.p9
                                )
                              )(fa.p10)
                            )(fa.p11)
                          )(fa.p12)
                        )(fa.p13)
                      )(fa.p14)
                    )(fa.p15)
                  )(fa.p16)
                )(fa.p17)
              )(fa.p18)
            )(fa.p19)
          )(fa.p20)
        )(fa.p21)
    }
}
case class Product22K[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](
    p1: F[T1],
    p2: F[T2],
    p3: F[T3],
    p4: F[T4],
    p5: F[T5],
    p6: F[T6],
    p7: F[T7],
    p8: F[T8],
    p9: F[T9],
    p10: F[T10],
    p11: F[T11],
    p12: F[T12],
    p13: F[T13],
    p14: F[T14],
    p15: F[T15],
    p16: F[T16],
    p17: F[T17],
    p18: F[T18],
    p19: F[T19],
    p20: F[T20],
    p21: F[T21],
    p22: F[T22]
)
object Product22K {
  implicit def findInstances[F[_], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](
      implicit p1: F[T1],
      p2: F[T2],
      p3: F[T3],
      p4: F[T4],
      p5: F[T5],
      p6: F[T6],
      p7: F[T7],
      p8: F[T8],
      p9: F[T9],
      p10: F[T10],
      p11: F[T11],
      p12: F[T12],
      p13: F[T13],
      p14: F[T14],
      p15: F[T15],
      p16: F[T16],
      p17: F[T17],
      p18: F[T18],
      p19: F[T19],
      p20: F[T20],
      p21: F[T21],
      p22: F[T22]
  ): Product22K[
    F,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
    T22
  ] =
    Product22K[F, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](
      p1,
      p2,
      p3,
      p4,
      p5,
      p6,
      p7,
      p8,
      p9,
      p10,
      p11,
      p12,
      p13,
      p14,
      p15,
      p16,
      p17,
      p18,
      p19,
      p20,
      p21,
      p22
    )

  implicit def product22KRepresentableTraverseInstance[
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      T22
  ]: RepresentableKC.Aux[Product22K[
    *[_],
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
    T12,
    T13,
    T14,
    T15,
    T16,
    T17,
    T18,
    T19,
    T20,
    T21,
    T22
  ], Const[Finite[22], *]]
    with TraverseKC[Product22K[
      *[_],
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      T22
    ]] =
    new RepresentableKC[Product22K[
      *[_],
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      T22
    ]] with TraverseKC[Product22K[
      *[_],
      T1,
      T2,
      T3,
      T4,
      T5,
      T6,
      T7,
      T8,
      T9,
      T10,
      T11,
      T12,
      T13,
      T14,
      T15,
      T16,
      T17,
      T18,
      T19,
      T20,
      T21,
      T22
    ]] {
      override type RepresentationK[A] = Finite[22]

      override def indexK[A[_], C](
          fa: Product22K[
            A,
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21,
            T22
          ]
      ): RepresentationK ~>: A = new (RepresentationK ~>: A) {
        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          case 0  => fa.p1.asInstanceOf[A[Z]]
          case 1  => fa.p2.asInstanceOf[A[Z]]
          case 2  => fa.p3.asInstanceOf[A[Z]]
          case 3  => fa.p4.asInstanceOf[A[Z]]
          case 4  => fa.p5.asInstanceOf[A[Z]]
          case 5  => fa.p6.asInstanceOf[A[Z]]
          case 6  => fa.p7.asInstanceOf[A[Z]]
          case 7  => fa.p8.asInstanceOf[A[Z]]
          case 8  => fa.p9.asInstanceOf[A[Z]]
          case 9  => fa.p10.asInstanceOf[A[Z]]
          case 10 => fa.p11.asInstanceOf[A[Z]]
          case 11 => fa.p12.asInstanceOf[A[Z]]
          case 12 => fa.p13.asInstanceOf[A[Z]]
          case 13 => fa.p14.asInstanceOf[A[Z]]
          case 14 => fa.p15.asInstanceOf[A[Z]]
          case 15 => fa.p16.asInstanceOf[A[Z]]
          case 16 => fa.p17.asInstanceOf[A[Z]]
          case 17 => fa.p18.asInstanceOf[A[Z]]
          case 18 => fa.p19.asInstanceOf[A[Z]]
          case 19 => fa.p20.asInstanceOf[A[Z]]
          case 20 => fa.p21.asInstanceOf[A[Z]]
          case 21 => fa.p22.asInstanceOf[A[Z]]
        }
      }

      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product22K[
        A,
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
        T17,
        T18,
        T19,
        T20,
        T21,
        T22
      ] =
        Product22K[
          A,
          T1,
          T2,
          T3,
          T4,
          T5,
          T6,
          T7,
          T8,
          T9,
          T10,
          T11,
          T12,
          T13,
          T14,
          T15,
          T16,
          T17,
          T18,
          T19,
          T20,
          T21,
          T22
        ](
          f(Finite(22, 0)),
          f(Finite(22, 1)),
          f(Finite(22, 2)),
          f(Finite(22, 3)),
          f(Finite(22, 4)),
          f(Finite(22, 5)),
          f(Finite(22, 6)),
          f(Finite(22, 7)),
          f(Finite(22, 8)),
          f(Finite(22, 9)),
          f(Finite(22, 10)),
          f(Finite(22, 11)),
          f(Finite(22, 12)),
          f(Finite(22, 13)),
          f(Finite(22, 14)),
          f(Finite(22, 15)),
          f(Finite(22, 16)),
          f(Finite(22, 17)),
          f(Finite(22, 18)),
          f(Finite(22, 19)),
          f(Finite(22, 20)),
          f(Finite(22, 21))
        )

      override def traverseK[G[_]: Applicative, A[_], B[_], C](
          fa: Product22K[
            A,
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21,
            T22
          ]
      )(f: A ~>: Compose2[G, B, *]): G[Product22K[
        B,
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
        T17,
        T18,
        T19,
        T20,
        T21,
        T22
      ]] =
        Applicative[G].map22(
          f(fa.p1),
          f(fa.p2),
          f(fa.p3),
          f(fa.p4),
          f(fa.p5),
          f(fa.p6),
          f(fa.p7),
          f(fa.p8),
          f(fa.p9),
          f(fa.p10),
          f(fa.p11),
          f(fa.p12),
          f(fa.p13),
          f(fa.p14),
          f(fa.p15),
          f(fa.p16),
          f(fa.p17),
          f(fa.p18),
          f(fa.p19),
          f(fa.p20),
          f(fa.p21),
          f(fa.p22)
        ) { (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22) =>
          Product22K[
            B,
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21,
            T22
          ](p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
        }

      override def foldLeftK[A[_], B, C](
          fa: Product22K[
            A,
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21,
            T22
          ],
          b: B
      )(f: B => A ~>#: B): B =
        f(
          f(
            f(
              f(
                f(
                  f(
                    f(
                      f(
                        f(
                          f(
                            f(
                              f(
                                f(
                                  f(f(f(f(f(f(f(f(f(b)(fa.p1))(fa.p2))(fa.p3))(fa.p4))(fa.p5))(fa.p6))(fa.p7))(fa.p8))(
                                    fa.p9
                                  )
                                )(fa.p10)
                              )(fa.p11)
                            )(fa.p12)
                          )(fa.p13)
                        )(fa.p14)
                      )(fa.p15)
                    )(fa.p16)
                  )(fa.p17)
                )(fa.p18)
              )(fa.p19)
            )(fa.p20)
          )(fa.p21)
        )(fa.p22)
    }
}

/*
object Creator extends App {

  (1 to 22).foreach { i =>
    val types            = (1 to i).map(i => s"T$i")
    val values           = (1 to i).map(i => s"p$i")
    val typesStr         = types.mkString(", ")
    val fTypesStr        = s"[F[_], $typesStr]"
    val fTypesStrApplied = s"[F, $typesStr]"
    val valuesStr        = values.zip(types).map { case (v, t) => s"$v: F[$t]" }.mkString(", ")

    println(s"case class Product${i}K$fTypesStr($valuesStr)")
    println(
      s"""|object Product${i}K {
          |  implicit def findInstances$fTypesStr(
          |    implicit $valuesStr
          |  ): Product${i}K$fTypesStrApplied = Product${i}K$fTypesStrApplied(${values.mkString(", ")})
          |
          |  implicit def product${i}KRepresentableTraverseInstance[$typesStr]
          |      : RepresentableKC.Aux[Product${i}K[*[_], $typesStr], Const[Finite[$i]]#λ] with TraverseKC[Product${i}K[*[_], $typesStr]] =
          |    new RepresentableKC[Product${i}K[*[_], $typesStr]] with TraverseKC[Product${i}K[*[_], $typesStr]] {
          |      override type RepresentationK[A] = Finite[$i]
          |
          |      override def indexK[A[_], C](fa: Product${i}K[A, $typesStr]): RepresentationK ~>: A = new (RepresentationK ~>: A) {
          |        override def apply[Z](i: RepresentationK[Z]): A[Z] = i.value match {
          |          ${(0 until i).map(i => s"case $i => fa.p${i + 1}.asInstanceOf[A[Z]]").mkString("\n          ")}
          |        }
          |      }
          |
          |      override def tabulateK[A[_], C](f: RepresentationK ~>: A): Product${i}K[A, $typesStr] =
          |        Product${i}K[A, $typesStr](
          |          ${(0 until i).map(j => s"f(Finite($i, $j))").mkString(", ")}
          |        )
          |
          |      override def traverseK[G[_] : Applicative, A[_], B[_], C](
          |        fa: Product${i}K[A, $typesStr]
          |      )(f: A ~>: Compose2[G, B, *]): G[Product${i}K[B, $typesStr]] =
          |        Applicative[G].${if (i == 1) "map" else s"map$i"}(${values.map(v => s"f(fa.$v)").mkString(", ")}) {
          |          (${values.mkString(", ")}) =>
          |            Product${i}K[B, $typesStr](${values.mkString(", ")})
          |        }
          |
          |      override def foldLeftK[A[_], B, C](fa: Product${i}K[A, $typesStr], b: B)(f: B => A ~>#: B): B =
          |        ${values.foldLeft("b")((acc, v) => s"f($acc)(fa.$v)")}
          |    }
          |}""".stripMargin
    )
  }
}
 */
