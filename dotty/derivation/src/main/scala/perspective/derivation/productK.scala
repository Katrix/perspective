package perspective.derivation

import cats.Applicative
import perspective._

object ProductK:

  transparent inline def fromSize(size: Int) = inline size match
    case 0 => Product0K
    case 1 => Product1K
    case 2 => Product2K
    case 3 => Product3K
    case 4 => Product4K
    case 5 => Product5K
    case 6 => Product6K
    case 7 => Product7K
    case 8 => Product8K
    case 9 => Product9K
    case 10 => Product10K
    case 11 => Product11K
    case 12 => Product12K
    case 13 => Product13K
    case 14 => Product14K
    case 15 => Product15K
    case 16 => Product16K
    case 17 => Product17K
    case 18 => Product18K
    case 19 => Product19K
    case 20 => Product20K
    case 21 => Product21K
    case 22 => Product22K
    
@main def generate: Unit = 
  for i <- 1 to 22 do
    val name = s"Product${i}K"
    val indices = 0 until i
    val typeParams = indices.map(i => s"T$i")
    val fTypeParams = typeParams.map(t => s"F[$t]")
    val valueParam = typeParams.map(_.toLowerCase)
    val valueTypeParams = valueParam.zip(typeParams).map((v, t) => s"$v: F[$t]")
    val tupleParam = indices.map(i => s"_${i + 1}")
    
    println(
      s"""|opaque type $name[${typeParams.mkString(", ")}, F[_]] = (${fTypeParams.mkString(", ")})
          |object $name:
          |  type Size = $i
          |  type Index[_] = Finite[Size]
          |  
          |  def fromTuple[F[_], ${typeParams.mkString(", ")}](t: (${fTypeParams.mkString(", ")})): $name[${typeParams.mkString(", ")}, F] =
          |    t
          |
          |  given findInstances[F[_], ${typeParams.mkString(", ")}](
          |    using ${valueTypeParams.mkString(", ")}
          |  ) as $name[${typeParams.mkString(", ")}, F] = (
          |    ${valueParam.mkString(", ")}
          |  )
          |
          |  private def instance[${typeParams.mkString(", ")}]: RepresentableKC.Aux[[F[_]] =>> $name[${typeParams.mkString(", ")}, F], Index] with TraverseKC[[F[_]] =>> $name[${typeParams.mkString(", ")}, F]] =
          |    new RepresentableKC[[F[_]] =>> $name[${typeParams.mkString(", ")}, F]] with TraverseKC[[F[_]] =>> $name[${typeParams.mkString(", ")}, F]]:
          |      type RepresentationK[A] = Finite[Size]
          |
          |      def indexK[A[_], C](fa: $name[${typeParams.mkString(", ")}, A]): RepresentationK ~>: A =
          |        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]
          |
          |      def tabulateK[A[_], C](f: RepresentationK ~>: A): $name[${typeParams.mkString(", ")}, A] =
          |        (${indices.map(j => s"f(Finite($i, $j))").mkString(", ")})
          |
          |      extension[G[_]: Applicative, A[_], B[_], C](fa: $name[${typeParams.mkString(", ")}, A]) def traverseK(f: A ~>: Compose2[G, B]): G[$name[${typeParams.mkString(", ")}, B]] =
          |        Applicative[G].${if i == 1 then "map" else s"map$i"}(${tupleParam.map(v => s"f(fa.$v)").mkString(", ")})(
          |          (${valueParam.mkString(", ")}) => (${valueParam.mkString(", ")})
          |        )
          |
          |      extension[A[_], B, C](fa: $name[${typeParams.mkString(", ")}, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
          |        //TODO Compiler crash
          |        //${tupleParam.foldLeft("b")((acc, v) => s"f($acc)(fa.$v)")}
          |        ???
          |
          |  given representable[${typeParams.mkString(", ")}] as RepresentableKC.Aux[[F[_]] =>> $name[${typeParams.mkString(", ")}, F], Index] = instance
          |  given traverse[${typeParams.mkString(", ")}] as TraverseKC[[F[_]] =>> $name[${typeParams.mkString(", ")}, F]] = instance
          |
          |""".stripMargin
    )

opaque type Product0K[F[_]] = Unit
object Product0K:
  type Size = 0
  type Index[_] = Nothing

  def fromTuple[F[_]](): Product0K[F] = ()

  given findInstances[F[_]] as Product0K[F] = ()

  private def instance[T <: Tuple]: RepresentableKC.Aux[Product0K, Index] with TraverseKC[Product0K] =
    new RepresentableKC[Product0K] with TraverseKC[Product0K]:
      type RepresentationK[A] = Nothing
  
      def indexK[A[_], C](fa: Product0K[A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => sys.error("impossible")
  
      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product0K[A] =
        ()
  
      extension[G[_]: Applicative, A[_], B[_], C](fa: Product0K[A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product0K[B]] =
        Applicative[G].pure(())
  
      extension[A[_], B, C](fa: Product0K[A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        b

  given representable as RepresentableKC.Aux[Product0K, Index] = instance
  given traverse as TraverseKC[Product0K] = instance

opaque type Product1K[T0, F[_]] = Tuple1[F[T0]]
object Product1K:
  type Size = 1
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0](t: Tuple1[F[T0]]): Product1K[T0, F] =
    t

  given findInstances[F[_], T0](
    using t0: F[T0]
  ) as Product1K[T0, F] = Tuple1(
    t0
  )

  private def instance[T0]: RepresentableKC.Aux[[F[_]] =>> Product1K[T0, F], Index] with TraverseKC[[F[_]] =>> Product1K[T0, F]] =
    new RepresentableKC[[F[_]] =>> Product1K[T0, F]] with TraverseKC[[F[_]] =>> Product1K[T0, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product1K[T0, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product1K[T0, A] =
        Tuple1(f(Finite(1, 0)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product1K[T0, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product1K[T0, B]] =
        Applicative[G].map(f(fa._1))(
          (t0) => Tuple1(t0)
        )

      extension[A[_], B, C](fa: Product1K[T0, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(b)(fa._1)
        ???

  given representable[T0] as RepresentableKC.Aux[[F[_]] =>> Product1K[T0, F], Index] = instance
  given traverse[T0] as TraverseKC[[F[_]] =>> Product1K[T0, F]] = instance


opaque type Product2K[T0, T1, F[_]] = (F[T0], F[T1])
object Product2K:
  type Size = 2
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1](t: (F[T0], F[T1])): Product2K[T0, T1, F] =
    t

  given findInstances[F[_], T0, T1](
    using t0: F[T0], t1: F[T1]
  ) as Product2K[T0, T1, F] = (
    t0, t1
  )

  private def instance[T0, T1]: RepresentableKC.Aux[[F[_]] =>> Product2K[T0, T1, F], Index] with TraverseKC[[F[_]] =>> Product2K[T0, T1, F]] =
    new RepresentableKC[[F[_]] =>> Product2K[T0, T1, F]] with TraverseKC[[F[_]] =>> Product2K[T0, T1, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product2K[T0, T1, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product2K[T0, T1, A] =
        (f(Finite(2, 0)), f(Finite(2, 1)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product2K[T0, T1, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product2K[T0, T1, B]] =
        Applicative[G].map2(f(fa._1), f(fa._2))(
          (t0, t1) => (t0, t1)
        )

      extension[A[_], B, C](fa: Product2K[T0, T1, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(b)(fa._1))(fa._2)
        ???

  given representable[T0, T1] as RepresentableKC.Aux[[F[_]] =>> Product2K[T0, T1, F], Index] = instance
  given traverse[T0, T1] as TraverseKC[[F[_]] =>> Product2K[T0, T1, F]] = instance


opaque type Product3K[T0, T1, T2, F[_]] = (F[T0], F[T1], F[T2])
object Product3K:
  type Size = 3
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2](t: (F[T0], F[T1], F[T2])): Product3K[T0, T1, T2, F] =
    t

  given findInstances[F[_], T0, T1, T2](
    using t0: F[T0], t1: F[T1], t2: F[T2]
  ) as Product3K[T0, T1, T2, F] = (
    t0, t1, t2
  )

  private def instance[T0, T1, T2]: RepresentableKC.Aux[[F[_]] =>> Product3K[T0, T1, T2, F], Index] with TraverseKC[[F[_]] =>> Product3K[T0, T1, T2, F]] =
    new RepresentableKC[[F[_]] =>> Product3K[T0, T1, T2, F]] with TraverseKC[[F[_]] =>> Product3K[T0, T1, T2, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product3K[T0, T1, T2, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product3K[T0, T1, T2, A] =
        (f(Finite(3, 0)), f(Finite(3, 1)), f(Finite(3, 2)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product3K[T0, T1, T2, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product3K[T0, T1, T2, B]] =
        Applicative[G].map3(f(fa._1), f(fa._2), f(fa._3))(
          (t0, t1, t2) => (t0, t1, t2)
        )

      extension[A[_], B, C](fa: Product3K[T0, T1, T2, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(b)(fa._1))(fa._2))(fa._3)
        ???

  given representable[T0, T1, T2] as RepresentableKC.Aux[[F[_]] =>> Product3K[T0, T1, T2, F], Index] = instance
  given traverse[T0, T1, T2] as TraverseKC[[F[_]] =>> Product3K[T0, T1, T2, F]] = instance


opaque type Product4K[T0, T1, T2, T3, F[_]] = (F[T0], F[T1], F[T2], F[T3])
object Product4K:
  type Size = 4
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3](t: (F[T0], F[T1], F[T2], F[T3])): Product4K[T0, T1, T2, T3, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3]
  ) as Product4K[T0, T1, T2, T3, F] = (
    t0, t1, t2, t3
  )

  private def instance[T0, T1, T2, T3]: RepresentableKC.Aux[[F[_]] =>> Product4K[T0, T1, T2, T3, F], Index] with TraverseKC[[F[_]] =>> Product4K[T0, T1, T2, T3, F]] =
    new RepresentableKC[[F[_]] =>> Product4K[T0, T1, T2, T3, F]] with TraverseKC[[F[_]] =>> Product4K[T0, T1, T2, T3, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product4K[T0, T1, T2, T3, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product4K[T0, T1, T2, T3, A] =
        (f(Finite(4, 0)), f(Finite(4, 1)), f(Finite(4, 2)), f(Finite(4, 3)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product4K[T0, T1, T2, T3, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product4K[T0, T1, T2, T3, B]] =
        Applicative[G].map4(f(fa._1), f(fa._2), f(fa._3), f(fa._4))(
          (t0, t1, t2, t3) => (t0, t1, t2, t3)
        )

      extension[A[_], B, C](fa: Product4K[T0, T1, T2, T3, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4)
        ???

  given representable[T0, T1, T2, T3] as RepresentableKC.Aux[[F[_]] =>> Product4K[T0, T1, T2, T3, F], Index] = instance
  given traverse[T0, T1, T2, T3] as TraverseKC[[F[_]] =>> Product4K[T0, T1, T2, T3, F]] = instance


opaque type Product5K[T0, T1, T2, T3, T4, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4])
object Product5K:
  type Size = 5
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4](t: (F[T0], F[T1], F[T2], F[T3], F[T4])): Product5K[T0, T1, T2, T3, T4, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4]
  ) as Product5K[T0, T1, T2, T3, T4, F] = (
    t0, t1, t2, t3, t4
  )

  private def instance[T0, T1, T2, T3, T4]: RepresentableKC.Aux[[F[_]] =>> Product5K[T0, T1, T2, T3, T4, F], Index] with TraverseKC[[F[_]] =>> Product5K[T0, T1, T2, T3, T4, F]] =
    new RepresentableKC[[F[_]] =>> Product5K[T0, T1, T2, T3, T4, F]] with TraverseKC[[F[_]] =>> Product5K[T0, T1, T2, T3, T4, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product5K[T0, T1, T2, T3, T4, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product5K[T0, T1, T2, T3, T4, A] =
        (f(Finite(5, 0)), f(Finite(5, 1)), f(Finite(5, 2)), f(Finite(5, 3)), f(Finite(5, 4)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product5K[T0, T1, T2, T3, T4, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product5K[T0, T1, T2, T3, T4, B]] =
        Applicative[G].map5(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5))(
          (t0, t1, t2, t3, t4) => (t0, t1, t2, t3, t4)
        )

      extension[A[_], B, C](fa: Product5K[T0, T1, T2, T3, T4, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5)
        ???

  given representable[T0, T1, T2, T3, T4] as RepresentableKC.Aux[[F[_]] =>> Product5K[T0, T1, T2, T3, T4, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4] as TraverseKC[[F[_]] =>> Product5K[T0, T1, T2, T3, T4, F]] = instance


opaque type Product6K[T0, T1, T2, T3, T4, T5, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5])
object Product6K:
  type Size = 6
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5])): Product6K[T0, T1, T2, T3, T4, T5, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5]
  ) as Product6K[T0, T1, T2, T3, T4, T5, F] = (
    t0, t1, t2, t3, t4, t5
  )

  private def instance[T0, T1, T2, T3, T4, T5]: RepresentableKC.Aux[[F[_]] =>> Product6K[T0, T1, T2, T3, T4, T5, F], Index] with TraverseKC[[F[_]] =>> Product6K[T0, T1, T2, T3, T4, T5, F]] =
    new RepresentableKC[[F[_]] =>> Product6K[T0, T1, T2, T3, T4, T5, F]] with TraverseKC[[F[_]] =>> Product6K[T0, T1, T2, T3, T4, T5, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product6K[T0, T1, T2, T3, T4, T5, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product6K[T0, T1, T2, T3, T4, T5, A] =
        (f(Finite(6, 0)), f(Finite(6, 1)), f(Finite(6, 2)), f(Finite(6, 3)), f(Finite(6, 4)), f(Finite(6, 5)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product6K[T0, T1, T2, T3, T4, T5, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product6K[T0, T1, T2, T3, T4, T5, B]] =
        Applicative[G].map6(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6))(
          (t0, t1, t2, t3, t4, t5) => (t0, t1, t2, t3, t4, t5)
        )

      extension[A[_], B, C](fa: Product6K[T0, T1, T2, T3, T4, T5, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6)
        ???

  given representable[T0, T1, T2, T3, T4, T5] as RepresentableKC.Aux[[F[_]] =>> Product6K[T0, T1, T2, T3, T4, T5, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5] as TraverseKC[[F[_]] =>> Product6K[T0, T1, T2, T3, T4, T5, F]] = instance


opaque type Product7K[T0, T1, T2, T3, T4, T5, T6, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6])
object Product7K:
  type Size = 7
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6])): Product7K[T0, T1, T2, T3, T4, T5, T6, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6]
  ) as Product7K[T0, T1, T2, T3, T4, T5, T6, F] = (
    t0, t1, t2, t3, t4, t5, t6
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6]: RepresentableKC.Aux[[F[_]] =>> Product7K[T0, T1, T2, T3, T4, T5, T6, F], Index] with TraverseKC[[F[_]] =>> Product7K[T0, T1, T2, T3, T4, T5, T6, F]] =
    new RepresentableKC[[F[_]] =>> Product7K[T0, T1, T2, T3, T4, T5, T6, F]] with TraverseKC[[F[_]] =>> Product7K[T0, T1, T2, T3, T4, T5, T6, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product7K[T0, T1, T2, T3, T4, T5, T6, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product7K[T0, T1, T2, T3, T4, T5, T6, A] =
        (f(Finite(7, 0)), f(Finite(7, 1)), f(Finite(7, 2)), f(Finite(7, 3)), f(Finite(7, 4)), f(Finite(7, 5)), f(Finite(7, 6)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product7K[T0, T1, T2, T3, T4, T5, T6, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product7K[T0, T1, T2, T3, T4, T5, T6, B]] =
        Applicative[G].map7(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7))(
          (t0, t1, t2, t3, t4, t5, t6) => (t0, t1, t2, t3, t4, t5, t6)
        )

      extension[A[_], B, C](fa: Product7K[T0, T1, T2, T3, T4, T5, T6, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6] as RepresentableKC.Aux[[F[_]] =>> Product7K[T0, T1, T2, T3, T4, T5, T6, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6] as TraverseKC[[F[_]] =>> Product7K[T0, T1, T2, T3, T4, T5, T6, F]] = instance


opaque type Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7])
object Product8K:
  type Size = 8
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7])): Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7]
  ) as Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7]: RepresentableKC.Aux[[F[_]] =>> Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F], Index] with TraverseKC[[F[_]] =>> Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F]] =
    new RepresentableKC[[F[_]] =>> Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F]] with TraverseKC[[F[_]] =>> Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product8K[T0, T1, T2, T3, T4, T5, T6, T7, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product8K[T0, T1, T2, T3, T4, T5, T6, T7, A] =
        (f(Finite(8, 0)), f(Finite(8, 1)), f(Finite(8, 2)), f(Finite(8, 3)), f(Finite(8, 4)), f(Finite(8, 5)), f(Finite(8, 6)), f(Finite(8, 7)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product8K[T0, T1, T2, T3, T4, T5, T6, T7, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product8K[T0, T1, T2, T3, T4, T5, T6, T7, B]] =
        Applicative[G].map8(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8))(
          (t0, t1, t2, t3, t4, t5, t6, t7) => (t0, t1, t2, t3, t4, t5, t6, t7)
        )

      extension[A[_], B, C](fa: Product8K[T0, T1, T2, T3, T4, T5, T6, T7, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7] as RepresentableKC.Aux[[F[_]] =>> Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7] as TraverseKC[[F[_]] =>> Product8K[T0, T1, T2, T3, T4, T5, T6, T7, F]] = instance


opaque type Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8])
object Product9K:
  type Size = 9
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8])): Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8]
  ) as Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8]: RepresentableKC.Aux[[F[_]] =>> Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F], Index] with TraverseKC[[F[_]] =>> Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F]] =
    new RepresentableKC[[F[_]] =>> Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F]] with TraverseKC[[F[_]] =>> Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, A] =
        (f(Finite(9, 0)), f(Finite(9, 1)), f(Finite(9, 2)), f(Finite(9, 3)), f(Finite(9, 4)), f(Finite(9, 5)), f(Finite(9, 6)), f(Finite(9, 7)), f(Finite(9, 8)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, B]] =
        Applicative[G].map9(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8) => (t0, t1, t2, t3, t4, t5, t6, t7, t8)
        )

      extension[A[_], B, C](fa: Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8] as RepresentableKC.Aux[[F[_]] =>> Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8] as TraverseKC[[F[_]] =>> Product9K[T0, T1, T2, T3, T4, T5, T6, T7, T8, F]] = instance


opaque type Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9])
object Product10K:
  type Size = 10
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9])): Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9]
  ) as Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]: RepresentableKC.Aux[[F[_]] =>> Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F], Index] with TraverseKC[[F[_]] =>> Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F]] =
    new RepresentableKC[[F[_]] =>> Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F]] with TraverseKC[[F[_]] =>> Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, A] =
        (f(Finite(10, 0)), f(Finite(10, 1)), f(Finite(10, 2)), f(Finite(10, 3)), f(Finite(10, 4)), f(Finite(10, 5)), f(Finite(10, 6)), f(Finite(10, 7)), f(Finite(10, 8)), f(Finite(10, 9)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, B]] =
        Applicative[G].map10(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)
        )

      extension[A[_], B, C](fa: Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9] as RepresentableKC.Aux[[F[_]] =>> Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9] as TraverseKC[[F[_]] =>> Product10K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, F]] = instance


opaque type Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10])
object Product11K:
  type Size = 11
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10])): Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10]
  ) as Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]: RepresentableKC.Aux[[F[_]] =>> Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F], Index] with TraverseKC[[F[_]] =>> Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F]] =
    new RepresentableKC[[F[_]] =>> Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F]] with TraverseKC[[F[_]] =>> Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, A] =
        (f(Finite(11, 0)), f(Finite(11, 1)), f(Finite(11, 2)), f(Finite(11, 3)), f(Finite(11, 4)), f(Finite(11, 5)), f(Finite(11, 6)), f(Finite(11, 7)), f(Finite(11, 8)), f(Finite(11, 9)), f(Finite(11, 10)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, B]] =
        Applicative[G].map11(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
        )

      extension[A[_], B, C](fa: Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] as RepresentableKC.Aux[[F[_]] =>> Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] as TraverseKC[[F[_]] =>> Product11K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, F]] = instance


opaque type Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11])
object Product12K:
  type Size = 12
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11])): Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11]
  ) as Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]: RepresentableKC.Aux[[F[_]] =>> Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F], Index] with TraverseKC[[F[_]] =>> Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F]] =
    new RepresentableKC[[F[_]] =>> Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F]] with TraverseKC[[F[_]] =>> Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, A] =
        (f(Finite(12, 0)), f(Finite(12, 1)), f(Finite(12, 2)), f(Finite(12, 3)), f(Finite(12, 4)), f(Finite(12, 5)), f(Finite(12, 6)), f(Finite(12, 7)), f(Finite(12, 8)), f(Finite(12, 9)), f(Finite(12, 10)), f(Finite(12, 11)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, B]] =
        Applicative[G].map12(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
        )

      extension[A[_], B, C](fa: Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] as RepresentableKC.Aux[[F[_]] =>> Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] as TraverseKC[[F[_]] =>> Product12K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, F]] = instance


opaque type Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12])
object Product13K:
  type Size = 13
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12])): Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12]
  ) as Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]: RepresentableKC.Aux[[F[_]] =>> Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F], Index] with TraverseKC[[F[_]] =>> Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F]] =
    new RepresentableKC[[F[_]] =>> Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F]] with TraverseKC[[F[_]] =>> Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, A] =
        (f(Finite(13, 0)), f(Finite(13, 1)), f(Finite(13, 2)), f(Finite(13, 3)), f(Finite(13, 4)), f(Finite(13, 5)), f(Finite(13, 6)), f(Finite(13, 7)), f(Finite(13, 8)), f(Finite(13, 9)), f(Finite(13, 10)), f(Finite(13, 11)), f(Finite(13, 12)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, B]] =
        Applicative[G].map13(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
        )

      extension[A[_], B, C](fa: Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] as RepresentableKC.Aux[[F[_]] =>> Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] as TraverseKC[[F[_]] =>> Product13K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, F]] = instance


opaque type Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13])
object Product14K:
  type Size = 14
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13])): Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13]
  ) as Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]: RepresentableKC.Aux[[F[_]] =>> Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F], Index] with TraverseKC[[F[_]] =>> Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F]] =
    new RepresentableKC[[F[_]] =>> Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F]] with TraverseKC[[F[_]] =>> Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, A] =
        (f(Finite(14, 0)), f(Finite(14, 1)), f(Finite(14, 2)), f(Finite(14, 3)), f(Finite(14, 4)), f(Finite(14, 5)), f(Finite(14, 6)), f(Finite(14, 7)), f(Finite(14, 8)), f(Finite(14, 9)), f(Finite(14, 10)), f(Finite(14, 11)), f(Finite(14, 12)), f(Finite(14, 13)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, B]] =
        Applicative[G].map14(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
        )

      extension[A[_], B, C](fa: Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] as RepresentableKC.Aux[[F[_]] =>> Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] as TraverseKC[[F[_]] =>> Product14K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, F]] = instance


opaque type Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14])
object Product15K:
  type Size = 15
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14])): Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13], t14: F[T14]
  ) as Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]: RepresentableKC.Aux[[F[_]] =>> Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F], Index] with TraverseKC[[F[_]] =>> Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F]] =
    new RepresentableKC[[F[_]] =>> Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F]] with TraverseKC[[F[_]] =>> Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, A] =
        (f(Finite(15, 0)), f(Finite(15, 1)), f(Finite(15, 2)), f(Finite(15, 3)), f(Finite(15, 4)), f(Finite(15, 5)), f(Finite(15, 6)), f(Finite(15, 7)), f(Finite(15, 8)), f(Finite(15, 9)), f(Finite(15, 10)), f(Finite(15, 11)), f(Finite(15, 12)), f(Finite(15, 13)), f(Finite(15, 14)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, B]] =
        Applicative[G].map15(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14), f(fa._15))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
        )

      extension[A[_], B, C](fa: Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14))(fa._15)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] as RepresentableKC.Aux[[F[_]] =>> Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] as TraverseKC[[F[_]] =>> Product15K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, F]] = instance


opaque type Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15])
object Product16K:
  type Size = 16
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15])): Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13], t14: F[T14], t15: F[T15]
  ) as Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]: RepresentableKC.Aux[[F[_]] =>> Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F], Index] with TraverseKC[[F[_]] =>> Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F]] =
    new RepresentableKC[[F[_]] =>> Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F]] with TraverseKC[[F[_]] =>> Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, A] =
        (f(Finite(16, 0)), f(Finite(16, 1)), f(Finite(16, 2)), f(Finite(16, 3)), f(Finite(16, 4)), f(Finite(16, 5)), f(Finite(16, 6)), f(Finite(16, 7)), f(Finite(16, 8)), f(Finite(16, 9)), f(Finite(16, 10)), f(Finite(16, 11)), f(Finite(16, 12)), f(Finite(16, 13)), f(Finite(16, 14)), f(Finite(16, 15)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, B]] =
        Applicative[G].map16(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14), f(fa._15), f(fa._16))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
        )

      extension[A[_], B, C](fa: Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14))(fa._15))(fa._16)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] as RepresentableKC.Aux[[F[_]] =>> Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] as TraverseKC[[F[_]] =>> Product16K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, F]] = instance


opaque type Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16])
object Product17K:
  type Size = 17
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16])): Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13], t14: F[T14], t15: F[T15], t16: F[T16]
  ) as Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]: RepresentableKC.Aux[[F[_]] =>> Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F], Index] with TraverseKC[[F[_]] =>> Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F]] =
    new RepresentableKC[[F[_]] =>> Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F]] with TraverseKC[[F[_]] =>> Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, A] =
        (f(Finite(17, 0)), f(Finite(17, 1)), f(Finite(17, 2)), f(Finite(17, 3)), f(Finite(17, 4)), f(Finite(17, 5)), f(Finite(17, 6)), f(Finite(17, 7)), f(Finite(17, 8)), f(Finite(17, 9)), f(Finite(17, 10)), f(Finite(17, 11)), f(Finite(17, 12)), f(Finite(17, 13)), f(Finite(17, 14)), f(Finite(17, 15)), f(Finite(17, 16)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, B]] =
        Applicative[G].map17(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14), f(fa._15), f(fa._16), f(fa._17))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
        )

      extension[A[_], B, C](fa: Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14))(fa._15))(fa._16))(fa._17)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] as RepresentableKC.Aux[[F[_]] =>> Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] as TraverseKC[[F[_]] =>> Product17K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, F]] = instance


opaque type Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17])
object Product18K:
  type Size = 18
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17])): Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13], t14: F[T14], t15: F[T15], t16: F[T16], t17: F[T17]
  ) as Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]: RepresentableKC.Aux[[F[_]] =>> Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F], Index] with TraverseKC[[F[_]] =>> Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F]] =
    new RepresentableKC[[F[_]] =>> Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F]] with TraverseKC[[F[_]] =>> Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, A] =
        (f(Finite(18, 0)), f(Finite(18, 1)), f(Finite(18, 2)), f(Finite(18, 3)), f(Finite(18, 4)), f(Finite(18, 5)), f(Finite(18, 6)), f(Finite(18, 7)), f(Finite(18, 8)), f(Finite(18, 9)), f(Finite(18, 10)), f(Finite(18, 11)), f(Finite(18, 12)), f(Finite(18, 13)), f(Finite(18, 14)), f(Finite(18, 15)), f(Finite(18, 16)), f(Finite(18, 17)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, B]] =
        Applicative[G].map18(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14), f(fa._15), f(fa._16), f(fa._17), f(fa._18))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
        )

      extension[A[_], B, C](fa: Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14))(fa._15))(fa._16))(fa._17))(fa._18)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] as RepresentableKC.Aux[[F[_]] =>> Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] as TraverseKC[[F[_]] =>> Product18K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, F]] = instance


opaque type Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17], F[T18])
object Product19K:
  type Size = 19
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17], F[T18])): Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13], t14: F[T14], t15: F[T15], t16: F[T16], t17: F[T17], t18: F[T18]
  ) as Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]: RepresentableKC.Aux[[F[_]] =>> Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F], Index] with TraverseKC[[F[_]] =>> Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F]] =
    new RepresentableKC[[F[_]] =>> Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F]] with TraverseKC[[F[_]] =>> Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, A] =
        (f(Finite(19, 0)), f(Finite(19, 1)), f(Finite(19, 2)), f(Finite(19, 3)), f(Finite(19, 4)), f(Finite(19, 5)), f(Finite(19, 6)), f(Finite(19, 7)), f(Finite(19, 8)), f(Finite(19, 9)), f(Finite(19, 10)), f(Finite(19, 11)), f(Finite(19, 12)), f(Finite(19, 13)), f(Finite(19, 14)), f(Finite(19, 15)), f(Finite(19, 16)), f(Finite(19, 17)), f(Finite(19, 18)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, B]] =
        Applicative[G].map19(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14), f(fa._15), f(fa._16), f(fa._17), f(fa._18), f(fa._19))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
        )

      extension[A[_], B, C](fa: Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14))(fa._15))(fa._16))(fa._17))(fa._18))(fa._19)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] as RepresentableKC.Aux[[F[_]] =>> Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] as TraverseKC[[F[_]] =>> Product19K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, F]] = instance


opaque type Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17], F[T18], F[T19])
object Product20K:
  type Size = 20
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17], F[T18], F[T19])): Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13], t14: F[T14], t15: F[T15], t16: F[T16], t17: F[T17], t18: F[T18], t19: F[T19]
  ) as Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]: RepresentableKC.Aux[[F[_]] =>> Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F], Index] with TraverseKC[[F[_]] =>> Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F]] =
    new RepresentableKC[[F[_]] =>> Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F]] with TraverseKC[[F[_]] =>> Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, A] =
        (f(Finite(20, 0)), f(Finite(20, 1)), f(Finite(20, 2)), f(Finite(20, 3)), f(Finite(20, 4)), f(Finite(20, 5)), f(Finite(20, 6)), f(Finite(20, 7)), f(Finite(20, 8)), f(Finite(20, 9)), f(Finite(20, 10)), f(Finite(20, 11)), f(Finite(20, 12)), f(Finite(20, 13)), f(Finite(20, 14)), f(Finite(20, 15)), f(Finite(20, 16)), f(Finite(20, 17)), f(Finite(20, 18)), f(Finite(20, 19)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, B]] =
        Applicative[G].map20(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14), f(fa._15), f(fa._16), f(fa._17), f(fa._18), f(fa._19), f(fa._20))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
        )

      extension[A[_], B, C](fa: Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14))(fa._15))(fa._16))(fa._17))(fa._18))(fa._19))(fa._20)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] as RepresentableKC.Aux[[F[_]] =>> Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] as TraverseKC[[F[_]] =>> Product20K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, F]] = instance


opaque type Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17], F[T18], F[T19], F[T20])
object Product21K:
  type Size = 21
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17], F[T18], F[T19], F[T20])): Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13], t14: F[T14], t15: F[T15], t16: F[T16], t17: F[T17], t18: F[T18], t19: F[T19], t20: F[T20]
  ) as Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]: RepresentableKC.Aux[[F[_]] =>> Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F], Index] with TraverseKC[[F[_]] =>> Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F]] =
    new RepresentableKC[[F[_]] =>> Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F]] with TraverseKC[[F[_]] =>> Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, A] =
        (f(Finite(21, 0)), f(Finite(21, 1)), f(Finite(21, 2)), f(Finite(21, 3)), f(Finite(21, 4)), f(Finite(21, 5)), f(Finite(21, 6)), f(Finite(21, 7)), f(Finite(21, 8)), f(Finite(21, 9)), f(Finite(21, 10)), f(Finite(21, 11)), f(Finite(21, 12)), f(Finite(21, 13)), f(Finite(21, 14)), f(Finite(21, 15)), f(Finite(21, 16)), f(Finite(21, 17)), f(Finite(21, 18)), f(Finite(21, 19)), f(Finite(21, 20)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, B]] =
        Applicative[G].map21(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14), f(fa._15), f(fa._16), f(fa._17), f(fa._18), f(fa._19), f(fa._20), f(fa._21))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
        )

      extension[A[_], B, C](fa: Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14))(fa._15))(fa._16))(fa._17))(fa._18))(fa._19))(fa._20))(fa._21)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] as RepresentableKC.Aux[[F[_]] =>> Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] as TraverseKC[[F[_]] =>> Product21K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, F]] = instance


opaque type Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F[_]] = (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17], F[T18], F[T19], F[T20], F[T21])
object Product22K:
  type Size = 22
  type Index[_] = Finite[Size]

  def fromTuple[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (F[T0], F[T1], F[T2], F[T3], F[T4], F[T5], F[T6], F[T7], F[T8], F[T9], F[T10], F[T11], F[T12], F[T13], F[T14], F[T15], F[T16], F[T17], F[T18], F[T19], F[T20], F[T21])): Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F] =
    t

  given findInstances[F[_], T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
    using t0: F[T0], t1: F[T1], t2: F[T2], t3: F[T3], t4: F[T4], t5: F[T5], t6: F[T6], t7: F[T7], t8: F[T8], t9: F[T9], t10: F[T10], t11: F[T11], t12: F[T12], t13: F[T13], t14: F[T14], t15: F[T15], t16: F[T16], t17: F[T17], t18: F[T18], t19: F[T19], t20: F[T20], t21: F[T21]
  ) as Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F] = (
    t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21
  )

  private def instance[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]: RepresentableKC.Aux[[F[_]] =>> Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F], Index] with TraverseKC[[F[_]] =>> Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F]] =
    new RepresentableKC[[F[_]] =>> Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F]] with TraverseKC[[F[_]] =>> Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F]]:
      type RepresentationK[A] = Finite[Size]

      def indexK[A[_], C](fa: Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, A]): RepresentationK ~>: A =
        [Z] => (r: RepresentationK[Z]) => fa.productElement(r.value).asInstanceOf[A[Z]]

      def tabulateK[A[_], C](f: RepresentationK ~>: A): Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, A] =
        (f(Finite(22, 0)), f(Finite(22, 1)), f(Finite(22, 2)), f(Finite(22, 3)), f(Finite(22, 4)), f(Finite(22, 5)), f(Finite(22, 6)), f(Finite(22, 7)), f(Finite(22, 8)), f(Finite(22, 9)), f(Finite(22, 10)), f(Finite(22, 11)), f(Finite(22, 12)), f(Finite(22, 13)), f(Finite(22, 14)), f(Finite(22, 15)), f(Finite(22, 16)), f(Finite(22, 17)), f(Finite(22, 18)), f(Finite(22, 19)), f(Finite(22, 20)), f(Finite(22, 21)))

      extension[G[_]: Applicative, A[_], B[_], C](fa: Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, A]) def traverseK(f: A ~>: Compose2[G, B]): G[Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, B]] =
        Applicative[G].map22(f(fa._1), f(fa._2), f(fa._3), f(fa._4), f(fa._5), f(fa._6), f(fa._7), f(fa._8), f(fa._9), f(fa._10), f(fa._11), f(fa._12), f(fa._13), f(fa._14), f(fa._15), f(fa._16), f(fa._17), f(fa._18), f(fa._19), f(fa._20), f(fa._21), f(fa._22))(
          (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) => (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)
        )

      extension[A[_], B, C](fa: Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, A]) def foldLeftK(b: B)(f: B => A ~>#: B): B =
        //TODO Compiler crash
        //f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(b)(fa._1))(fa._2))(fa._3))(fa._4))(fa._5))(fa._6))(fa._7))(fa._8))(fa._9))(fa._10))(fa._11))(fa._12))(fa._13))(fa._14))(fa._15))(fa._16))(fa._17))(fa._18))(fa._19))(fa._20))(fa._21))(fa._22)
        ???

  given representable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] as RepresentableKC.Aux[[F[_]] =>> Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F], Index] = instance
  given traverse[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] as TraverseKC[[F[_]] =>> Product22K[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, F]] = instance