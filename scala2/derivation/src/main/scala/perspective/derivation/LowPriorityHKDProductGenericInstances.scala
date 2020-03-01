package perspective.derivation

import cats.{Applicative, Id}
import shapeless.{Const => _, _}
import perspective._
import shapeless.labelled._

trait LowPriorityHKDProductGenericInstances {

  implicit def productHListFromShapeless[Prod, Gen <: HList, GenHKD[A[_]] <: HList](
      implicit gen: LabelledGeneric.Aux[Prod, Gen],
      createHKDGen: CreateHKDGen[Gen, GenHKD]
  ): HKDProductGeneric.Aux[Prod, GenHKD] = new HKDProductGeneric[Prod] {
    override type Gen[A[_]] = GenHKD[A]

    override def names: GenHKD[Const[String, *]] = createHKDGen.names

    override def to(a: Prod): GenHKD[Id] = createHKDGen.to(gen.to(a))

    override def from(prod: GenHKD[Id]): Prod = gen.from(createHKDGen.from(prod))

    override def representable: RepresentableKC[GenHKD] = createHKDGen.representable

    override def traverse: TraverseKC[GenHKD] = createHKDGen.traverse
  }
}

trait CreateHKDGen[Gen <: HList, GenHKD[A[_]] <: HList] {
  type Size <: Nat
  def size: Int

  def names: GenHKD[Const[String, *]]

  def to(gen: Gen): GenHKD[Id]

  def from(gen: GenHKD[Id]): Gen

  //Would be great if we could easily convert Nat to Finite, and use that
  def representable: RepresentableKC.Aux[GenHKD, Const[Int, *]]

  def traverse: TraverseKC[GenHKD]
}
object CreateHKDGen {

  implicit val hNilHKDGen: CreateHKDGen[HNil, λ[A[_] => HNil]] = new CreateHKDGen[HNil, λ[A[_] => HNil]] {
    type Size = _0
    def size: Int = 0

    override def names: HNil = HNil

    override def to(gen: HNil): HNil = HNil

    override def from(gen: HNil): HNil = HNil

    override def representable: RepresentableKC.Aux[λ[A[_] => HNil], Const[Int, *]] =
      new RepresentableKC[λ[A[_] => HNil]] {
        override type RepresentationK[A] = Int

        override def indexK[A[_], C](fa: HNil): RepresentationK ~>: A = new (RepresentationK ~>: A) {
          override def apply[Z](fa: RepresentationK[Z]): A[Z] = sys.error("impossible")
        }

        override def tabulateK[A[_], C](f: RepresentationK ~>: A): HNil = HNil
      }

    override def traverse: TraverseKC[λ[A[_] => HNil]] = new TraverseKC[λ[A[_] => HNil]] {
      override def traverseK[G[_]: Applicative, A[_], B[_], C](fa: HNil)(f: A ~>: Compose2[G, B, *]): G[HNil] =
        Applicative[G].pure(HNil)

      override def foldLeftK[A[_], B, C](fa: HNil, b: B)(f: B => A ~>#: B): B = b
    }
  }

  implicit def hConsHKDGen[H, K <: Symbol, T <: HList, THKD[_[_]] <: HList](
      implicit tailHKD: CreateHKDGen[T, THKD],
      k: ValueOf[K]
  ): CreateHKDGen[FieldType[K, H] :: T, λ[A[_] => A[H] :: THKD[A]]] =
    new CreateHKDGen[FieldType[K, H] :: T, λ[A[_] => A[H] :: THKD[A]]] {
      type Size = Succ[tailHKD.Size]
      def size: Int = tailHKD.size + 1

      override def names: String :: THKD[Const[String, *]] = k.value.name :: tailHKD.names

      override def to(gen: FieldType[K, H] :: T): Id[H] :: THKD[Id] = gen.head :: tailHKD.to(gen.tail)

      override def from(gen: Id[H] :: THKD[Id]): FieldType[K, H] :: T = field[K](gen.head) :: tailHKD.from(gen.tail)

      override def representable: RepresentableKC.Aux[λ[A[_] => A[H] :: THKD[A]], Const[Int, *]] =
        new RepresentableKC[λ[A[_] => A[H] :: THKD[A]]] {
          override type RepresentationK[A] = Int

          override def indexK[A[_], C](fa: A[H] :: THKD[A]): RepresentationK ~>: A = new (RepresentationK ~>: A) {
            override def apply[Z](i: RepresentationK[Z]): A[Z] =
              if (i == size) fa.head.asInstanceOf[A[Z]] else tailHKD.representable.indexK(fa.tail).apply(i)
          }

          override def tabulateK[A[_], C](f: RepresentationK ~>: A): A[H] :: THKD[A] =
            f(size).asInstanceOf[A[H]] :: tailHKD.representable.tabulateK(f)
        }

      override def traverse: TraverseKC[λ[A[_] => A[H] :: THKD[A]]] = new TraverseKC[λ[A[_] => A[H] :: THKD[A]]] {
        override def traverseK[G[_]: Applicative, A[_], B[_], C](fa: A[H] :: THKD[A])(
            f: A ~>: Compose2[G, B, *]
        ): G[B[H] :: THKD[B]] = Applicative[G].map2(f(fa.head), tailHKD.traverse.traverseK(fa.tail)(f)) { (h, t) =>
          h :: t
        }

        override def foldLeftK[A[_], B, C](fa: A[H] :: THKD[A], b: B)(f: B => A ~>#: B): B =
          tailHKD.traverse.foldLeftK(fa.tail, f(b)(fa.head))(f)
      }
    }
}
