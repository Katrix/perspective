package perspective.derivation

import scala.language.implicitConversions

import scala.compiletime._
import scala.deriving._
import scala.quoted._

import perspective._

sealed trait HKDGeneric[A]:
  type Gen[_[_]]
  type Index[A]
  type ElemTop

  class IdxWrapper[X](val idx: Index[X])
  given [X]: Conversion[IdxWrapper[X], Index[X]] = _.idx
  given [X]: Conversion[Index[X], IdxWrapper[X]] = new IdxWrapper(_)

  inline def upcastIndex[X](idx: Index[X]): IdxWrapper[_ <: ElemTop] =
    new IdxWrapper(idx).asInstanceOf[IdxWrapper[_ <: ElemTop]]

  type TypeName <: String
  def typeName: TypeName

  type Names <: String
  def names: Gen[Const[Names]]
  def stringToName(s: String): Option[Names]

  type FieldOf[Name <: Names] <: ElemTop
  def nameToIndex[Name <: Names](name: Name): Index[FieldOf[Name]]

  type TupleRep <: Tuple
  def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F]
  def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F]

  lazy val representable: RepresentableKC.Aux[Gen, Index]
  lazy val traverse: TraverseKC[Gen]

  given RepresentableKC.Aux[Gen, Index] = representable
  given TraverseKC[Gen]                 = traverse

object HKDGeneric:
  type Aux[A, Gen0[_[_]]] = HKDGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  type TupleUnionLub[T <: Tuple, Lub, Acc <: Lub] <: Lub = T match {
    case (h & Lub) *: t => TupleUnionLub[t, Lub, Acc | h]
    case EmptyTuple     => Acc
  }

  type FieldOfImpl[Name <: String, ElemTop, ElemTypes <: Tuple, Labels <: Tuple] <: ElemTop =
    (ElemTypes, Labels, Labels) match {
      case (th *: tt, Name *: lt, lh *: _) =>
        compiletime.ops.any.==[lh, Name] match {
          case true => th & ElemTop
        }
      case (_ *: tt, _ *: lt, _) => FieldOfImpl[Name, ElemTop, tt, lt]
    }

  transparent inline given derived[A](using m: Mirror.Of[A])(
      using ValueOf[Tuple.Size[m.MirroredElemTypes]],
      Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDGeneric[A] = inline m match
    case m: Mirror.ProductOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =>
      HKDProductGeneric.derived[A](using m)

    case m: Mirror.SumOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =>
      HKDSumGeneric.derived[A](using m)
end HKDGeneric

trait HKDProductGeneric[A] extends HKDGeneric[A]:
  def to(a: A): Gen[Id]
  def from(gen: Gen[Id]): A

object HKDProductGeneric:
  transparent inline def apply[A](using gen: HKDProductGeneric[A]): HKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  transparent inline given derived[A](using m: Mirror.ProductOf[A])(
      using ValueOf[Tuple.Size[m.MirroredElemTypes]],
      Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDProductGeneric[A] =
    type Names = HKDGeneric.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel, Names](
      constValue[m.MirroredLabel],
      Helpers.constValueTupleOptimized[m.MirroredElemLabels].asInstanceOf[Tuple.Map[m.MirroredElemTypes, Const[Names]]],
      Helpers.constValueTupleOptimized[m.MirroredElemLabels].toList.toSet.asInstanceOf[Set[String]]
    )

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String, NamesUnion <: String](
      label: Label,
      namesTuple: Tuple.Map[ElemTypes, Const[NamesUnion]],
      namesSet: Set[String]
  )(
      using m: Mirror.ProductOf[A] { type MirroredElemTypes = ElemTypes; type MirroredLabel = Label }
  )(
      using ValueOf[Tuple.Size[m.MirroredElemTypes]],
      Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDProductGeneric[A] {
    type Gen[F[_]]              = ProductK[F, m.MirroredElemTypes]
    type Index[_]               = Finite[Tuple.Size[m.MirroredElemTypes]]
    type TypeName               = Label
    type Names                  = NamesUnion
    type ElemTop                = Tuple.Union[ElemTypes]
    type FieldOf[Name <: Names] = HKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, m.MirroredElemLabels]
    type TupleRep               = m.MirroredElemTypes
  } =
    new HKDProductGeneric[A]:
      override type Gen[F[_]] = ProductK[F, m.MirroredElemTypes]
      override type Index[_]  = Finite[Tuple.Size[m.MirroredElemTypes]]
      override type ElemTop   = Tuple.Union[ElemTypes]

      override type TypeName = Label
      override def typeName: TypeName = label

      override type Names = NamesUnion
      override def names: Gen[Const[Names]] =
        ProductK.of[Const[Names], m.MirroredElemTypes](namesTuple)

      override def stringToName(s: String): Option[NamesUnion] =
        Option.when(namesSet(s))(s.asInstanceOf[Names])

      override type FieldOf[Name <: Names] = HKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, m.MirroredElemLabels]

      private lazy val nameMap =
        names
          .map2Const(representable.indicesK)([Z] => (name: Names, idx: Index[Z]) => (name, upcastIndex(idx)))
          .toListK
          .toMap

      override def nameToIndex[Name <: Names](name: Name): Index[FieldOf[Name]] = nameMap(name)

      override type TupleRep = m.MirroredElemTypes
      override def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F]   = gen.tuple
      override def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F] = ProductK.of(tuple)

      override def to(a: A): Gen[Id] =
        ProductK.of[Id, m.MirroredElemTypes](
          Tuple.fromProduct(a.asInstanceOf[Product]).asInstanceOf[Tuple.Map[m.MirroredElemTypes, Id]]
        )
      override def from(a: Gen[Id]): A =
        m.fromProduct(a.tuple)

      private lazy val instance: RepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
        ProductK.productKInstance[m.MirroredElemTypes]

      override lazy val representable: RepresentableKC.Aux[Gen, Index] = instance
      override lazy val traverse: TraverseKC[Gen]                      = instance

trait HKDSumGeneric[A] extends HKDGeneric[A]:
  override type ElemTop <: A
  type NameOf[X <: ElemTop] <: Names

  def indexOf[X <: ElemTop](x: X): Index[X]

  def indexOfA(a: A): IdxWrapper[_ <: ElemTop] = indexOf(a.asInstanceOf[ElemTop])

  def indexOfACasting(a: A): HKDSumGeneric.IndexOfACasting[Index, ElemTop]

  def indexToName[X <: ElemTop](idx: Index[X]): NameOf[X]

  inline def widenConst[F[+_]](gen: Gen[F]): Gen[Const[F[A]]] =
    // This is safe. We can't use the widen method as it can't know about the contents of Gen, we do
    gen.asInstanceOf[Gen[Const[F[A]]]]

  def to(a: A): Gen[Option] =
    val index = indexOfA(a)
    // This cast is safe as we know A = Z
    representable.tabulateK([Z] => (i: Index[Z]) => if i == index.idx then Some(a.asInstanceOf[Z]) else None)

  def from(a: Gen[Option]): Option[A] =
    traverse.toListK(widenConst(a)).flatten match
      case Nil      => None    // No values present
      case a :: Nil => Some(a) // One value present
      case _        => None    // More than one value present

object HKDSumGeneric:
  def apply[A](using gen: HKDSumGeneric[A]): HKDSumGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDSumGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  trait IndexOfACasting[Index[_], ElemTop] {
    type X0 <: ElemTop
    val index: Index[X0]
    val value: X0
  }

  object IndexOfACasting {
    class IndexOfACastingImpl[Index[_], ElemTop, X1 <: ElemTop](
        val index: Index[X1],
        val value: X1
    ) extends IndexOfACasting[Index, ElemTop] {
      type X0 = X1
    }
  }

  type NameOfImpl[Names, X, ElemTypes, Labels] <: Names = (ElemTypes, ElemTypes, Labels) match {
    case (X *: tt, th *: _, lh *: lt) =>
      Helpers.Eq[th, X] match {
        case true => lh & Names
      }
    case (th *: tt, _, lh *: lt) => NameOfImpl[Names, X, tt, lt]
  }

  transparent inline given derived[A](using m: Mirror.SumOf[A])(
      using ValueOf[Tuple.Size[m.MirroredElemTypes]],
      Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDSumGeneric[A] =
    type Names = HKDGeneric.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel, Names](
      constValue[m.MirroredLabel],
      Helpers.constValueTupleOptimized[m.MirroredElemLabels].asInstanceOf[Tuple.Map[m.MirroredElemTypes, Const[Names]]],
      Helpers.constValueTupleOptimized[m.MirroredElemLabels].toList.toSet.asInstanceOf[Set[String]],
      constValue[Tuple.Size[m.MirroredElemTypes]]
    )

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String, NamesUnion <: String](
      label: Label,
      namesTuple: Tuple.Map[ElemTypes, Const[NamesUnion]],
      namesSet: Set[String],
      size: Tuple.Size[ElemTypes]
  )(
      using m: Mirror.SumOf[A] { type MirroredElemTypes = ElemTypes; type MirroredLabel = Label }
  )(
      using ValueOf[Tuple.Size[ElemTypes]],
      Finite.NotZero[Tuple.Size[ElemTypes]] =:= true
  ): HKDSumGeneric[A] {
    type Gen[F[_]]              = ProductK[F, ElemTypes]
    type Index[_]               = Finite[Tuple.Size[ElemTypes]]
    type TypeName               = Label
    type Names                  = NamesUnion
    type ElemTop                = HKDGeneric.TupleUnionLub[ElemTypes, A, Nothing]
    type FieldOf[Name <: Names] = HKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, m.MirroredElemLabels]
    type NameOf[X <: ElemTop]   = NameOfImpl[Names, X, ElemTypes, m.MirroredElemLabels]
    type TupleRep               = ElemTypes
  } =
    new HKDSumGeneric[A]:
      type Gen[F[_]] = ProductK[F, ElemTypes]
      type Index[_]  = Finite[Tuple.Size[ElemTypes]]
      type ElemTop   = HKDGeneric.TupleUnionLub[ElemTypes, A, Nothing]

      override type TypeName = Label
      override def typeName: TypeName = label

      override type Names = NamesUnion
      override def names: Gen[Const[Names]] =
        ProductK.of[Const[Names], ElemTypes](namesTuple)

      override def stringToName(s: String): Option[NamesUnion] =
        Option.when(namesSet(s))(s.asInstanceOf[Names])

      override type FieldOf[Name <: Names] = HKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, m.MirroredElemLabels]
      override type NameOf[X <: ElemTop]   = NameOfImpl[Names, X, ElemTypes, m.MirroredElemLabels]

      private lazy val nameMap =
        names
          .map2K[Index, Const[(Names, IdxWrapper[_ <: ElemTop])]](representable.indicesK)(
            [Z] => (name: Names, idx: Index[Z]) => (name, upcastIndex(idx))
          )
          .toListK
          .toMap

      override def nameToIndex[Name <: Names](name: Name): Index[FieldOf[Name]] = nameMap(name)
      override def indexToName[X <: ElemTop](idx: Index[X]): NameOf[X] = names.indexK(idx).asInstanceOf[NameOf[X]]

      override def indexOf[X <: ElemTop](x: X): Index[X] = Finite(size, m.ordinal(x))

      override def indexOfACasting(a: A): HKDSumGeneric.IndexOfACasting[Index, ElemTop] =
        val idx = indexOfA(a)
        new HKDSumGeneric.IndexOfACasting.IndexOfACastingImpl[Index, ElemTop, ElemTop](idx, a.asInstanceOf[ElemTop])

      override type TupleRep = ElemTypes
      override def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F]   = gen.tuple
      override def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F] = ProductK.of(tuple)

      private lazy val instance: RepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
        ProductK.productKInstance[m.MirroredElemTypes]

      override lazy val representable: RepresentableKC.Aux[Gen, Index] = instance
      override lazy val traverse: TraverseKC[Gen]                      = instance
