package perspective.derivation

import scala.language.implicitConversions

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*

import cats.Applicative
import cats.kernel.{BoundedEnumerable, Order}
import perspective.*

trait GenHKDGeneric[A]:
  type Cat[_]

  /** A representation of [[A]] supporting higher kinded types. */
  type Gen[_[_]]

  /** The index of the [[Gen]] type. */
  type Index[B]

  /** The top type for the inner type of [[Index]] and [[Gen]]. */
  type ElemTop

  /** A wrapper for [[Index]] where we want wildcards of it. */
  case class IdxWrapper[X](idx: Index[X])
  given [X]: Conversion[IdxWrapper[X], Index[X]] = _.idx
  given [X]: Conversion[Index[X], IdxWrapper[X]] = IdxWrapper(_)

  /** Upcast an index to its bound. */
  inline def upcastIndex[X](idx: Index[X]): IdxWrapper[_ <: ElemTop] =
    IdxWrapper(idx).asInstanceOf[IdxWrapper[_ <: ElemTop]]

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

  /** Given a index, return the name of the index. */
  def indexToName[X](idx: Index[X]): Names = names.indexK(idx)

  /** Validates a string as a name if it matches the name of a field. */
  def stringToName(s: String): Option[Names]

  /** Returns the index of the field a name corresponds to. */
  def nameToIndex(name: Names): IdxWrapper[_ <: ElemTop]

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

  val representable: BoundedRepresentableKC.Aux[Gen, Index]
  val traverse: TraverseKC[Gen]

  given BoundedRepresentableKC.Aux[Gen, Index] = representable
  given TraverseKC[Gen]                        = traverse

  export representable.*
  export traverse.{asK => _, liftK => _, mapConst => _, mapK => _, voidK => _, widen => _, *}

  // Cat generic functions

  /** Convert a value of `Cat[A]` to the higher kinded representation. */
  def catTo(a: Cat[A]): Gen[Cat]

  /** Convert a value of the higher kinded representation to `Cat[A]`. */
  def catFrom(gen: Gen[Cat]): Cat[A]

  extension (a: Cat[A]) def productElementCat[X](index: Index[X]): Cat[X]

  // Extra ops

  def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B

  def tabulateTraverseK[G[_], B[_]](f: [X] => Index[X] => G[B[X]])(using Applicative[G]): G[Gen[B]]

  def tabulateTraverseKOption[B[_]](f: [X] => Index[X] => Option[B[X]]): Option[Gen[B]]

  def tabulateTraverseKEither[E, B[_]](f: [X] => Index[X] => Either[E, B[X]]): Either[E, Gen[B]]

/**
  * A type somewhat like [[Mirror.Of]] allowing manipulating a type as if it was
  * defined as a higher kinded type.
  * @tparam A
  *   The type being abstracted over.
  */
sealed trait HKDGeneric[A] extends GenHKDGeneric[A]

object HKDGeneric:
  type Aux[A, Gen0[_[_]]] = HKDGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  transparent inline given derived[A](using m: Mirror.Of[A], typeLength: TypeLength[A])(
      using Finite.NotZero[typeLength.Length] =:= true
  ): HKDGeneric[A] = inline m match
    case m: Mirror.ProductOf[A] =>
      HKDProductGeneric.derived(using m)

    case m: Mirror.SumOf[A] =>
      HKDSumGeneric.derived[A](using m)

  private[derivation] def tabulateFoldLeftImpl[N <: Int, B](size: N, start: B, f: B => [X] => Finite[N] => B): B =
    var res: B = start
    var i: Int = 0
    while i < size do
      res = f(res)(Finite.unsafeApply(i))
      i += 1

    res

  private[derivation] def makeArrayObjectArray[A](arr: Array[A]): Array[Object] = arr match
    case arr: Array[Object] => arr
    case _                  => arr.map(Helpers.boxAny(_))

  private[derivation] def tabulateTraverseKImpl[N <: Int, ElemTop, T <: Tuple, Gen[_[_]], G[_], B[_]](
      size: N,
      f: [X] => Finite[N] => G[B[X]]
  )(
      using G: Applicative[G]
  ): G[ProductK[B, T]] =
    var acc: G[List[B[ElemTop]]] = G.pure(List.empty[B[ElemTop]])
    var i: Int                   = 0
    while (i < size) {
      acc = G.map2(f[ElemTop](Finite.unsafeApply(i)), acc)((v, a) => v :: a)
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
      val res = f[ElemTop](Finite.unsafeApply(i))

      if res.isEmpty
      then
        // Early return
        return None
      else arr(i) = Helpers.boxAny(res.get)

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
      val res = f[ElemTop](Finite.unsafeApply(i))
      res match
        case Right(v) =>
          arr(i) = Helpers.boxAny(v)
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
trait GenHKDProductGeneric[A] extends GenHKDGeneric[A]:

  /** Convert a value of [[A]] to the higher kinded representation. */
  def to(a: Cat[A]): Gen[Cat] = catTo(a)

  /** Convert a value of the higher kinded representation to [[A]]. */
  def from(gen: Gen[Cat]): Cat[A] = catFrom(gen)

/**
  * A type somewhat like [[Mirror.ProductOf]] allowing manipulating a product
  * type as if it was defined as a higher kinded type.
  * @tparam A
  *   The type being abstracted over.
  */
trait HKDProductGeneric[A] extends GenHKDProductGeneric[A] with HKDGeneric[A]:
  type Cat[B] = B

  extension (a: A) def productElementId[X](index: Index[X]): X = a.productElementCat(index)

object HKDProductGeneric:
  transparent inline def apply[A](using gen: HKDProductGeneric[A]): HKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = HKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  transparent inline given derived[A](using m: Mirror.ProductOf[A], typeLength: TypeLength[A]): HKDProductGeneric[A] =
    val labels = Helpers.constValueTupleToIArray[m.MirroredElemLabels, String]
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel, typeLength.Length](
      constValue[m.MirroredLabel],
      labels,
      labels.toSet
    )

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String, L <: Int](
      label: Label,
      namesArr: IArray[String],
      namesSet: Set[String]
  )(
      using m: Mirror.ProductOf[A] {
        type MirroredElemTypes = ElemTypes; type MirroredLabel = Label
      },
      typeLength: TypeLength.Aux[A, L]
  ): HKDProductGeneric[A] {
    type TupleRep  = ElemTypes
    type Gen[F[_]] = ProductK[F, TupleRep]
    type Index[_]  = Finite[typeLength.Length]
    type TypeName  = Label
  } =
    new HKDProductGeneric[A]:
      override type Gen[F[_]] = ProductK[F, TupleRep]
      override type Index[_]  = Finite[typeLength.Length]

      override type TypeName = Label
      override def typeName: TypeName = label

      opaque type Names <: String = String
      override def names: Gen[Const[Names]] =
        ProductK.ofProductUnsafe[Const[Names], TupleRep](ArrayProduct(namesArr))

      override def stringToName(s: String): Option[Names] =
        Option.when(namesSet(s))(s)

      private lazy val nameMap = namesArr.zipWithIndex.toMap

      override def nameToIndex(name: Names): IdxWrapper[_ <: ElemTop] = IdxWrapper(Finite.unsafeApply(nameMap(name)))

      override type TupleRep = ElemTypes
      override def genToTuple[F[_]](gen: Gen[F]): Helpers.TupleMap[TupleRep, F]   = gen.tuple
      override def tupleToGen[F[_]](tuple: Helpers.TupleMap[TupleRep, F]): Gen[F] = ProductK.ofTuple(tuple)

      override def catTo(a: A): Gen[Id] = ProductK.ofProductUnsafe(a.asInstanceOf[Product])

      override def catFrom(a: Gen[Id]): A =
        m.fromProduct(a.product)

      override def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B =
        HKDGeneric.tabulateFoldLeftImpl(typeLength.length, start, f)

      override def tabulateTraverseK[G[_], B[_]](f: [X] => Finite[typeLength.Length] => G[B[X]])(
          using Applicative[G]
      ): G[Gen[B]] =
        HKDGeneric.tabulateTraverseKImpl(typeLength.length, f)

      override def tabulateTraverseKOption[B[_]](
          f: [X] => Finite[typeLength.Length] => Option[B[X]]
      ): Option[ProductK[B, TupleRep]] = HKDGeneric.tabulateTraverseKOptionImpl(typeLength.length, f)

      override def tabulateTraverseKEither[E, B[_]](
          f: [X] => Finite[typeLength.Length] => Either[E, B[X]]
      ): Either[E, ProductK[B, TupleRep]] = HKDGeneric.tabulateTraverseKEitherImpl(typeLength.length, f)

      extension (a: A)
        def productElementCat[X](index: Index[X]): X =
          (a.asInstanceOf[Product].productElement(index.value): Any).asInstanceOf[X]

      private val instance: BoundedRepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
        ProductK.productKInstance[TupleRep](using typeLength.asInstanceOf[TypeLength.Aux[TupleRep, typeLength.Length]])

      override val representable: BoundedRepresentableKC.Aux[Gen, Index] = instance
      override val traverse: TraverseKC[Gen]                             = instance

trait GenHKDSumGeneric[A] extends GenHKDGeneric[A]:
  override type ElemTop <: A

/**
  * A type somewhat like [[Mirror.SumOf]] allowing manipulating a sum type as if
  * it was defined as a higher kinded type.
  * @tparam A
  *   The type being abstracted over.
  */
trait HKDSumGeneric[A] extends HKDGeneric[A]:
  type Cat[B] = Option[B]

  override type ElemTop <: A

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
  def to(a: A): Gen[Option] = catTo(Some(a))

  override def catTo(ao: Option[A]): Gen[Option] =
    ao.fold(
      representable.pure([Z] => () => None: Option[Z])
    ) { a =>
      val index = indexOf((a: A).asInstanceOf[ElemTop])
      // This cast is safe as we know A = Z
      representable.tabulateK([Z] => (i: Index[Z]) => if i == index.idx then Some((a: A).asInstanceOf[Z]) else None)
    }

  /**
    * Convert a value of the higher kinded representation to [[A]]. Will only
    * return Some if only one of the fields is Some and the rest is None.
    */
  def from(a: Gen[Option]): Option[A] = catFrom(a)

  def catFrom(a: Gen[Option]): Option[A] =
    traverse.toListK(widenConst(a)).flatten match
      case Nil      => None    // No values present
      case a :: Nil => Some(a) // One value present
      case _        => None    // More than one value present

  extension (ao: Option[A])
    def productElementCat[X](index: Index[X]): Option[X] = ao.flatMap { a =>
      val aIdx = indexOfA(a)
      if index == aIdx.idx then Some((a: A).asInstanceOf[X]) else None
    }

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

  transparent inline given derived[A](using m: Mirror.SumOf[A], typeLength: TypeLength[A])(
      using Finite.NotZero[typeLength.Length] =:= true
  ): HKDSumGeneric[A] =
    val labels = Helpers.constValueTupleToIArray[m.MirroredElemLabels, String]
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel](
      constValue[m.MirroredLabel],
      labels,
      labels.toSet
    )

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String](
      label: Label,
      namesArr: IArray[String],
      namesSet: Set[String]
  )(
      using m: Mirror.SumOf[A] {
        type MirroredElemTypes = ElemTypes; type MirroredLabel = Label;
      },
      typeLength: TypeLength[A],
      nz: Finite.NotZero[typeLength.Length] =:= true
  ): HKDSumGeneric[A] {
    type TupleRep  = ElemTypes
    type Gen[F[_]] = ProductK[F, TupleRep]
    type Index[_]  = Finite[typeLength.Length]
    type TypeName  = Label
  } =
    new HKDSumGeneric[A]:
      type Gen[F[_]] = ProductK[F, TupleRep]
      type Index[_]  = Finite[typeLength.Length]

      override type TypeName = Label
      override def typeName: TypeName = label

      opaque type Names <: String = String
      override def names: Gen[Const[Names]] = ProductK.ofProductUnsafe(ArrayProduct(namesArr))

      override def stringToName(s: String): Option[Names] = Option.when(namesSet(s))(s)

      private lazy val nameMap = namesArr.zipWithIndex.toMap

      override def nameToIndex(name: Names): IdxWrapper[_ <: ElemTop] = IdxWrapper(Finite.unsafeApply(nameMap(name)))

      override def indexOf[X <: ElemTop](x: X): Index[X] = Finite(typeLength.length, m.ordinal(x))

      override def indexOfACasting(a: A): HKDSumGeneric.IndexOfACasting[Index, ElemTop] =
        val idx = indexOfA(a)
        new HKDSumGeneric.IndexOfACasting.IndexOfACastingImpl[Index, ElemTop, ElemTop](idx, a.asInstanceOf[ElemTop])

      override def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B =
        HKDGeneric.tabulateFoldLeftImpl(typeLength.length, start, f)

      override def tabulateTraverseK[G[_], B[_]](f: [X] => Finite[typeLength.Length] => G[B[X]])(
          using Applicative[G]
      ): G[Gen[B]] =
        HKDGeneric.tabulateTraverseKImpl(typeLength.length, f)

      override def tabulateTraverseKOption[B[_]](
          f: [X] => Finite[typeLength.Length] => Option[B[X]]
      ): Option[ProductK[B, TupleRep]] = HKDGeneric.tabulateTraverseKOptionImpl(typeLength.length, f)

      override def tabulateTraverseKEither[E, B[_]](
          f: [X] => Finite[typeLength.Length] => Either[E, B[X]]
      ): Either[E, ProductK[B, TupleRep]] = HKDGeneric.tabulateTraverseKEitherImpl(typeLength.length, f)

      override type TupleRep = ElemTypes
      override def genToTuple[F[_]](gen: Gen[F]): Helpers.TupleMap[TupleRep, F]   = gen.tuple
      override def tupleToGen[F[_]](tuple: Helpers.TupleMap[TupleRep, F]): Gen[F] = ProductK.ofTuple(tuple)

      private val instance: BoundedRepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
        ProductK.productKInstance[m.MirroredElemTypes](
          using typeLength.asInstanceOf[TypeLength.Aux[m.MirroredElemTypes, typeLength.Length]]
        )

      override val representable: BoundedRepresentableKC.Aux[Gen, Index] = instance
      override val traverse: TraverseKC[Gen]                             = instance
