package perspective.derivation

import scala.language.implicitConversions

import scala.Tuple.Size
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

import cats.Applicative
import perspective.*

/**
  * A type somewhat like [[Mirror.Of]] allowing manipulating a type as if it was
  * defined as a higher kinded type.
  * @tparam A
  *   The type being abstracted over.
  */
sealed trait HKDGeneric[A]:
  /** A representation of [[A]] supporting higher kinded types. */
  type Gen[_[_]]

  /** The index of the [[Gen]] type. */
  type Index[A]

  /** The top type for the inner type of [[Index]] and [[Gen]]. */
  type ElemTop

  /** A wrapper for [[Index]] where we want wildcards of it. */
  class IdxWrapper[X](val idx: Index[X])
  given [X]: Conversion[IdxWrapper[X], Index[X]] = _.idx
  given [X]: Conversion[Index[X], IdxWrapper[X]] = new IdxWrapper(_)

  /** Upcast an index to its bound. */
  inline def upcastIndex[X](idx: Index[X]): IdxWrapper[_ <: ElemTop] =
    new IdxWrapper(idx).asInstanceOf[IdxWrapper[_ <: ElemTop]]

  /** The name of the [[A]] type. */
  type TypeName <: String

  /** The name of the [[A]] type. */
  def typeName: TypeName

  /**
    * The name of the fields of [[A]] type. Field in this case can mean either
    * the children of a sum type, or the fields of a product type.
    */
  type Names <: String

  /**
    * The name of the fields of [[A]] type. Field in this case can mean either
    * the children of a sum type, or the fields of a product type.
    */
  def names: Gen[Const[Names]]

  /** Validates a string as a name if it matches the name of a field. */
  def stringToName(s: String): Option[Names]

  /** Returns the type of a field given its name. */
  type FieldOf[Name <: Names] <: ElemTop

  /** Returns the index of the field a name corresponds to. */
  def nameToIndex[Name <: Names](name: Name): Index[FieldOf[Name]]

  /** A tuple representation of [[A]]. */
  type TupleRep <: Tuple

  /** Converts [[Gen]] to the tuple representation. */
  def genToTuple[F[_]](gen: Gen[F]): Helpers.TupleMap[TupleRep, F]

  /** Converts the tuple representation to Gen. */
  def tupleToGen[F[_]](tuple: Helpers.TupleMap[TupleRep, F]): Gen[F]

  /** Converts [[Gen]] to the scala tuple representation. */
  def genToScalaTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F] = genToTuple(gen).asInstanceOf[Tuple.Map[TupleRep, F]]

  /** Converts the scala tuple representation to Gen. */
  def scalaTupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F] = tupleToGen(
    tuple.asInstanceOf[Helpers.TupleMap[TupleRep, F]]
  )

  val representable: RepresentableKC.Aux[Gen, Index]
  val traverse: TraverseKC[Gen]

  given RepresentableKC.Aux[Gen, Index] = representable
  given TraverseKC[Gen]                 = traverse

  export representable.*
  export traverse.{mapK => _, liftK => _, widen => _, voidK => _, mapConst => _, asK => _, *}

  // Extra ops

  def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B

  def tabulateTraverseK[G[_], B[_]](f: [X] => Index[X] => G[B[X]])(using Applicative[G]): G[Gen[B]]

  def tabulateTraverseKOption[B[_]](f: [X] => Index[X] => Option[B[X]]): Option[Gen[B]]

  def tabulateTraverseKEither[E, B[_]](f: [X] => Index[X] => Either[E, B[X]]): Either[E, Gen[B]]

object HKDGeneric:
  type Aux[A, Gen0[_[_]]] = HKDGeneric[A] {
    type Gen[B[_]] = Gen0[B]
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
      HKDProductGeneric.derived(using m)

    case m: Mirror.SumOf[A] { type MirroredElemTypes = m.MirroredElemTypes } =>
      HKDSumGeneric.derived[A](using m)

  private[derivation] def tabulateFoldLeftImpl[N <: Int, B](size: N, start: B, f: B => [X] => Finite[N] => B): B =
    var res: B = start
    var i: Int = 0
    while i < size do
      res = f(res)(i.asInstanceOf[Finite[N]])
      i += 1

    res

  private[derivation] def makeArrayObjectArray[A](arr: Array[A]): Array[Object] = arr match
    case arr: Array[Object] => arr
    case _                  => arr.map(_.asInstanceOf[Object])

  private[derivation] def tabulateTraverseKImpl[N <: Int, ElemTop, T <: Tuple, Gen[_[_]], G[_], B[_]](
      size: N,
      f: [X] => Finite[N] => G[B[X]]
  )(
      using G: Applicative[G]
  ): G[ProductK[B, T]] =
    var acc: G[List[B[ElemTop]]] = G.pure(List.empty[B[ElemTop]])
    var i: Int                   = 0
    while (i < size) {
      acc = G.map2(f[ElemTop](i.asInstanceOf[Finite[N]]), acc)((v, a) => v :: a)
      i += 1
    }

    G.map(acc) { a =>
      val objArr = makeArrayObjectArray(a.reverseIterator.toArray)
      ProductK.ofProductUnsafe(ArrayProduct.ofArrayUnsafe(objArr))
    }

  private[derivation] def tabulateTraverseKOptionImpl[N <: Int, ElemTop, T <: Tuple, B[_]](
      size: N,
      f: [X] => Finite[N] => Option[B[X]]
  ): Option[ProductK[B, T]] =
    val arr    = new Array[Object](size)
    var i: Int = 0
    while (i < size) {
      val res = f[ElemTop](i.asInstanceOf[Finite[N]])

      if res.isEmpty
      then
        // Early return
        return None
      else arr(i) = res.get.asInstanceOf[Object]

      i += 1
    }

    Some(ProductK.ofProductUnsafe(ArrayProduct.ofArrayUnsafe(arr)))

  private[derivation] def tabulateTraverseKEitherImpl[N <: Int, ElemTop, T <: Tuple, E, B[_]](
      size: N,
      f: [X] => Finite[N] => Either[E, B[X]]
  ): Either[E, ProductK[B, T]] =
    val arr    = new Array[Object](size)
    var i: Int = 0
    while (i < size) {
      val res = f[ElemTop](i.asInstanceOf[Finite[N]])
      res match
        case Right(v) =>
          arr(i) = v.asInstanceOf[Object]
        case Left(e) =>
          // Early return
          return Left(e)

      i += 1
    }

    Right(ProductK.ofProductUnsafe(ArrayProduct.ofArrayUnsafe(arr)))

end HKDGeneric

/**
  * A type somewhat like [[Mirror.ProductOf]] allowing manipulating a product
  * type as if it was defined as a higher kinded type.
  * @tparam A
  *   The type being abstracted over.
  */
trait HKDProductGeneric[A] extends HKDGeneric[A]:

  /** Convert a value of [[A]] to the higher kinded representation. */
  def to(a: A): Gen[Id]

  /** Convert a value of the higher kinded representation to [[A]]. */
  def from(gen: Gen[Id]): A

  extension (a: A) def productElementId[X](index: Index[X]): X

object HKDProductGeneric:
  transparent inline def apply[A](using gen: HKDProductGeneric[A]): HKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  transparent inline given derived[A](using m: Mirror.ProductOf[A])(
      using ValueOf[Tuple.Size[m.MirroredElemTypes]]
  ): HKDProductGeneric[A] =
    val labels = Helpers.constValueTupleToIArray[m.MirroredElemLabels, String]
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel, m.MirroredElemLabels](
      constValue[m.MirroredLabel],
      labels,
      labels.toSet
    )

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String, Labels <: Tuple](
      label: Label,
      namesArr: IArray[String],
      namesSet: Set[String]
  )(
      using m: Mirror.ProductOf[A] {
        type MirroredElemTypes = ElemTypes; type MirroredLabel = Label; type MirroredElemLabels = Labels
      },
      size: ValueOf[Tuple.Size[m.MirroredElemTypes]]
  ): HKDProductGeneric[A] {
    type Gen[F[_]]              = ProductK[F, ElemTypes]
    type Index[_]               = Finite[Tuple.Size[ElemTypes]]
    type TypeName               = Label
    type Names                  = Helpers.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
    type ElemTop                = Helpers.TupleUnion[ElemTypes, Nothing]
    type FieldOf[Name <: Names] = HKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, m.MirroredElemLabels]
    type TupleRep               = ElemTypes
  } =
    new HKDProductGeneric[A]:
      override type Gen[F[_]] = ProductK[F, ElemTypes]
      override type Index[_]  = Finite[Tuple.Size[ElemTypes]]
      override type ElemTop   = Helpers.TupleUnion[ElemTypes, Nothing]

      override type TypeName = Label
      override def typeName: TypeName = label

      override type Names = Helpers.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
      override def names: Gen[Const[Names]] =
        ProductK.ofProductUnsafe[Const[Names], ElemTypes](ArrayProduct(namesArr))

      override def stringToName(s: String): Option[Names] =
        Option.when(namesSet(s))(s.asInstanceOf[Names])

      override type FieldOf[Name <: Names] = HKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, m.MirroredElemLabels]

      private lazy val nameMap =
        names
          .map2Const(representable.indicesK)([Z] => (name: Names, idx: Index[Z]) => (name, upcastIndex(idx)))
          .toListK
          .toMap

      override def nameToIndex[Name <: Names](name: Name): Index[FieldOf[Name]] = nameMap(name)

      override type TupleRep = ElemTypes
      override def genToTuple[F[_]](gen: Gen[F]): Helpers.TupleMap[TupleRep, F]   = gen.tuple
      override def tupleToGen[F[_]](tuple: Helpers.TupleMap[TupleRep, F]): Gen[F] = ProductK.ofTuple(tuple)

      override def to(a: A): Gen[Id] = ProductK.ofProductUnsafe(a.asInstanceOf[Product])

      override def from(a: Gen[Id]): A =
        m.fromProduct(a.product)

      override def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B =
        HKDGeneric.tabulateFoldLeftImpl(size.value, start, f)

      override def tabulateTraverseK[G[_], B[_]](f: [X] => Finite[Size[ElemTypes]] => G[B[X]])(
          using Applicative[G]
      ): G[Gen[B]] =
        HKDGeneric.tabulateTraverseKImpl(size.value, f)

      override def tabulateTraverseKOption[B[_]](
          f: [X] => Finite[Size[ElemTypes]] => Option[B[X]]
      ): Option[ProductK[B, ElemTypes]] = HKDGeneric.tabulateTraverseKOptionImpl(size.value, f)

      override def tabulateTraverseKEither[E, B[_]](
          f: [X] => Finite[Size[ElemTypes]] => Either[E, B[X]]
      ): Either[E, ProductK[B, ElemTypes]] = HKDGeneric.tabulateTraverseKEitherImpl(size.value, f)

      extension (a: A)
        def productElementId[X](index: Index[X]): X =
          a.asInstanceOf[Product].productElement(index.value).asInstanceOf[X]

      private val instance: RepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
        ProductK.productKInstance[ElemTypes]

      override val representable: RepresentableKC.Aux[Gen, Index] = instance
      override val traverse: TraverseKC[Gen]                      = instance

/**
  * A type somewhat like [[Mirror.SumOf]] allowing manipulating a sum type as if
  * it was defined as a higher kinded type.
  * @tparam A
  *   The type being abstracted over.
  */
trait HKDSumGeneric[A] extends HKDGeneric[A]:
  override type ElemTop <: A

  /** Returns the name of a field given the type of the field. */
  type NameOf[X <: ElemTop] <: Names

  /**
    * Returns the index of a value. Because of soundness, this method can not be
    * used if X = A. In that case, use [[indexOfA]] instead.
    */
  def indexOf[X <: ElemTop](x: X): Index[X]

  /** Same as [[indexOf]] but also works for values of type A. */
  def indexOfA(a: A): IdxWrapper[_ <: ElemTop] = indexOf(a.asInstanceOf[ElemTop])

  /**
    * Same as [[indexOfA]] but also essentially casts the value to the unknown
    * type, allowing further operations on it that requires that it is a subtype
    * of A.
    */
  def indexOfACasting(a: A): HKDSumGeneric.IndexOfACasting[Index, ElemTop]

  /** Given a index, return the name of the index. */
  def indexToName[X <: ElemTop](idx: Index[X]): NameOf[X]

  /**
    * Widen the higher kinded representation to a [[Const]] type of the top
    * type.
    */
  inline def widenConst[F[+_]](gen: Gen[F]): Gen[Const[F[A]]] =
    // This is safe. We can't use the widen method as it can't know about the contents of Gen, we do
    gen.asInstanceOf[Gen[Const[F[A]]]]

  /**
    * Convert a value of [[A]] to the higher kinded representation. It will be
    * Some in only one field, corresponding to the subtype passed in, and None
    * in all the others.
    */
  def to(a: A): Gen[Option] =
    val index = indexOf(a.asInstanceOf[ElemTop])
    // This cast is safe as we know A = Z
    representable.tabulateK([Z] => (i: Index[Z]) => if i == index.idx then Some(a.asInstanceOf[Z]) else None)

  /**
    * Convert a value of the higher kinded representation to [[A]]. Will only
    * return Some if only one of the fields is Some and the rest is None.
    */
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
    val labels = Helpers.constValueTupleToIArray[m.MirroredElemLabels, String]
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel, m.MirroredElemLabels](
      constValue[m.MirroredLabel],
      labels,
      labels.toSet
    )

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String, Labels <: Tuple](
      label: Label,
      namesArr: IArray[String],
      namesSet: Set[String]
  )(
      using m: Mirror.SumOf[A] {
        type MirroredElemTypes = ElemTypes; type MirroredLabel = Label; type MirroredElemLabels = Labels
      },
      size: ValueOf[Tuple.Size[ElemTypes]],
      nz: Finite.NotZero[Tuple.Size[m.MirroredElemTypes]] =:= true
  ): HKDSumGeneric[A] {
    type Gen[F[_]]              = ProductK[F, ElemTypes]
    type Index[_]               = Finite[Tuple.Size[ElemTypes]]
    type TypeName               = Label
    type Names                  = Helpers.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
    type ElemTop                = Helpers.TupleUnionLub[ElemTypes, A, Nothing]
    type FieldOf[Name <: Names] = HKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, m.MirroredElemLabels]
    type NameOf[X <: ElemTop]   = NameOfImpl[Names, X, ElemTypes, m.MirroredElemLabels]
    type TupleRep               = ElemTypes
  } =
    new HKDSumGeneric[A]:
      type Gen[F[_]] = ProductK[F, ElemTypes]
      type Index[_]  = Finite[Tuple.Size[ElemTypes]]
      type ElemTop   = Helpers.TupleUnionLub[ElemTypes, A, Nothing]

      override type TypeName = Label
      override def typeName: TypeName = label

      override type Names = Helpers.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
      override def names: Gen[Const[Names]] =
        ProductK.ofProductUnsafe(ArrayProduct(namesArr))

      override def stringToName(s: String): Option[Names] =
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

      override def indexOf[X <: ElemTop](x: X): Index[X] = Finite(size.value, m.ordinal(x))

      override def indexOfACasting(a: A): HKDSumGeneric.IndexOfACasting[Index, ElemTop] =
        val idx = indexOfA(a)
        new HKDSumGeneric.IndexOfACasting.IndexOfACastingImpl[Index, ElemTop, ElemTop](idx, a.asInstanceOf[ElemTop])

      override def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B =
        HKDGeneric.tabulateFoldLeftImpl(size.value, start, f)

      override def tabulateTraverseK[G[_], B[_]](f: [X] => Finite[Size[ElemTypes]] => G[B[X]])(
          using Applicative[G]
      ): G[Gen[B]] =
        HKDGeneric.tabulateTraverseKImpl(size.value, f)

      override def tabulateTraverseKOption[B[_]](
          f: [X] => Finite[Size[ElemTypes]] => Option[B[X]]
      ): Option[ProductK[B, ElemTypes]] = HKDGeneric.tabulateTraverseKOptionImpl(size.value, f)

      override def tabulateTraverseKEither[E, B[_]](
          f: [X] => Finite[Size[ElemTypes]] => Either[E, B[X]]
      ): Either[E, ProductK[B, ElemTypes]] = HKDGeneric.tabulateTraverseKEitherImpl(size.value, f)

      override type TupleRep = ElemTypes
      override def genToTuple[F[_]](gen: Gen[F]): Helpers.TupleMap[TupleRep, F]   = gen.tuple
      override def tupleToGen[F[_]](tuple: Helpers.TupleMap[TupleRep, F]): Gen[F] = ProductK.ofTuple(tuple)

      private val instance: RepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
        ProductK.productKInstance[m.MirroredElemTypes]

      override val representable: RepresentableKC.Aux[Gen, Index] = instance
      override val traverse: TraverseKC[Gen]                      = instance
