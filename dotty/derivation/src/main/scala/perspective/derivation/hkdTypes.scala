package perspective.derivation

import scala.annotation.unused
import scala.deriving.Mirror

import perspective.*

sealed trait HKDExtraTypes[A]:
  type Gen[F[_]]
  type Index[_]
  type TupleRep <: Tuple

  type Names <: String
  def names: Gen[Const[Names]]

  def stringToName(s: String): Option[Names]

  def nameToIndex[Name <: Names](name: Name): Index[FieldOf[Name]]

  type ElemTop
  type FieldOf[Name <: Names]

  type Special

  def withIdx[I[_]](using @unused ev: Index[Special] =:= I[Special]): this.type {
    type Index[B] = I[B]
  } = this.asInstanceOf[
    this.type {
      type Index[B] = I[B]
    }
  ]

object HKDExtraTypes:
  transparent inline given derived[A](using m: Mirror.Of[A], typeLength: TypeLength[m.MirroredElemTypes])(
      using Finite.NotZero[typeLength.Length] =:= true
  ): HKDExtraTypes[A] = inline m match
    case m: Mirror.ProductOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =>
      HKDProductExtraTypes.derived(using m)

    case m: Mirror.SumOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =>
      HKDSumExtraTypes.derived[A](using m)

  type FieldOfImpl[Name <: String, ElemTop, ElemTypes <: Tuple, Labels <: Tuple] <: ElemTop =
    (ElemTypes, Labels, Labels) match {
      case (th *: tt, Name *: lt, lh *: _) =>
        compiletime.ops.any.==[lh, Name] match {
          case true => th & ElemTop
        }
      case (_ *: tt, _ *: lt, _) => FieldOfImpl[Name, ElemTop, tt, lt]
    }
end HKDExtraTypes

trait HKDProductExtraTypes[A] extends HKDExtraTypes[A]
object HKDProductExtraTypes:

  transparent inline given derived[A](
      using m: Mirror.ProductOf[A],
      typeLength: TypeLength[m.MirroredElemTypes]
  ): HKDProductExtraTypes[A] =
    val labels = Helpers.constValueTupleToIArray[m.MirroredElemLabels, String]
    derivedImpl[A, m.MirroredElemTypes, m.MirroredElemLabels](
      labels,
      labels.toSet
    )

  def derivedImpl[A, ElemTypes <: Tuple, Labels <: Tuple](
      namesArr: IArray[String],
      namesSet: Set[String]
  )(
      using m: Mirror.ProductOf[A] {
        type MirroredElemTypes = ElemTypes; type MirroredElemLabels = Labels
      },
      typeLength: TypeLength[m.MirroredElemTypes]
  ): HKDProductExtraTypes[A] {
    type TupleRep               = ElemTypes
    type Gen[F[_]]              = ProductK[F, TupleRep]
    type Index[_]               = Finite[typeLength.Length]
    type Names                  = Helpers.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
    type ElemTop                = Helpers.TupleUnion[TupleRep, Nothing]
    type FieldOf[Name <: Names] = HKDExtraTypes.FieldOfImpl[Name, ElemTop, TupleRep, m.MirroredElemLabels]
  } =
    new HKDProductExtraTypes[A] {
      override type TupleRep               = ElemTypes
      override type Gen[F[_]]              = ProductK[F, TupleRep]
      override type Index[_]               = Finite[typeLength.Length]
      override type Names                  = Helpers.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
      override type ElemTop                = Helpers.TupleUnion[TupleRep, Nothing]
      override type FieldOf[Name <: Names] = HKDExtraTypes.FieldOfImpl[Name, ElemTop, TupleRep, m.MirroredElemLabels]

      override def names: Gen[Const[Names]] =
        ProductK.ofProductUnsafe[Const[Names], TupleRep](ArrayProduct(namesArr))

      override def stringToName(s: String): Option[Names] =
        Option.when(namesSet(s))(s.asInstanceOf[Names])

      private lazy val nameMap = namesArr.zipWithIndex.map(t => (t._1, t._2)).toMap

      override def nameToIndex[Name <: Names](name: Name): Index[FieldOf[Name]] =
        Finite.unsafeApply(nameMap(name))
    }
end HKDProductExtraTypes

trait HKDSumExtraTypes[A] extends HKDExtraTypes[A]:
  override type ElemTop <: A

  /** Returns the name of a field given the type of the field. */
  type NameOf[X <: ElemTop] <: Names

  /** Given a index, return the name of the index. */
  def indexToName[X <: ElemTop](idx: Index[X]): NameOf[X]

object HKDSumExtraTypes:

  type NameOfImpl[Names, X, ElemTypes, Labels] <: Names = (ElemTypes, ElemTypes, Labels) match {
    case (X *: tt, th *: _, lh *: lt) =>
      Helpers.Eq[th, X] match {
        case true => lh & Names
      }
    case (th *: tt, _, lh *: lt) => NameOfImpl[Names, X, tt, lt]
  }

  transparent inline given derived[A](
      using m: Mirror.SumOf[A],
      typeLength: TypeLength[m.MirroredElemTypes]
  ): HKDSumExtraTypes[A] =
    val labels = Helpers.constValueTupleToIArray[m.MirroredElemLabels, String]
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel](
      labels,
      labels.toSet
    )

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String](
      namesArr: IArray[String],
      namesSet: Set[String]
  )(
      using m: Mirror.SumOf[A] {
        type MirroredElemTypes = ElemTypes; type MirroredLabel = Label
      },
      typeLength: TypeLength[m.MirroredElemTypes]
  ): HKDSumExtraTypes[A] {
    type TupleRep               = ElemTypes
    type Gen[F[_]]              = ProductK[F, TupleRep]
    type Index[_]               = Finite[typeLength.Length]
    type Names                  = Helpers.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
    type ElemTop                = Helpers.TupleUnionLub[TupleRep, A, Nothing]
    type FieldOf[Name <: Names] = HKDExtraTypes.FieldOfImpl[Name, ElemTop, TupleRep, m.MirroredElemLabels]
  } = new HKDSumExtraTypes[A]:
    override type TupleRep = ElemTypes
    type Gen[F[_]]         = ProductK[F, TupleRep]
    type Index[_]          = Finite[typeLength.Length]
    type ElemTop           = Helpers.TupleUnionLub[TupleRep, A, Nothing]

    override type Names = Helpers.TupleUnionLub[m.MirroredElemLabels, String, Nothing]

    override def names: Gen[Const[Names]] = ProductK.ofProductUnsafe(ArrayProduct(namesArr))

    override def stringToName(s: String): Option[Names] = Option.when(namesSet(s))(s.asInstanceOf[Names])

    override type FieldOf[Name <: Names] = HKDExtraTypes.FieldOfImpl[Name, ElemTop, TupleRep, m.MirroredElemLabels]

    private lazy val nameMap = namesArr.zipWithIndex.toMap

    override def nameToIndex[Name <: Names](name: Name): Index[FieldOf[Name]] =
      Finite.unsafeApply(nameMap(name))

    override type NameOf[X <: ElemTop] = HKDSumExtraTypes.NameOfImpl[Names, X, ElemTypes, m.MirroredElemLabels]

    override def indexToName[X <: ElemTop](idx: Index[X]): NameOf[X] =
      (namesArr(idx.value): String).asInstanceOf[NameOf[X]]
