package perspective.derivation

import scala.language.implicitConversions

import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

import cats.Applicative
import cats.kernel.{BoundedEnumerable, Order}
import perspective.*

sealed trait ExprHKDGeneric[A] extends GenHKDGeneric[A]:

  def types: Gen[Type]

  def summonInstancesOpt[F[_]: Type]: Option[Gen[Compose2[Expr, F]]]

object ExprHKDGeneric

trait ExprHKDProductGeneric[A] extends GenHKDProductGeneric[A] with ExprHKDGeneric[A]:
  type Cat[B] = Expr[B]
object ExprHKDProductGeneric:

  transparent inline given derived[A](using q: Quotes, aType: Type[A]): ExprHKDProductGeneric[A] =
    Expr.summon[Mirror.ProductOf[A]].get match
      case '{
            type elemTypes <: Tuple
            type label <: String
            type labels <: Tuple
            $m: Mirror.ProductOf[A] {
              type MirroredElemTypes  = `elemTypes`
              type MirroredLabel      = `label`
              type MirroredElemLabels = `labels`
            }
          } =>
        import q.reflect.*
        val labels  = Type.valueOfTuple[labels].get.toIArray.map(e => (e: Any).asInstanceOf[String])
        val label   = Type.valueOfConstant[label].get
        val lengthV = labels.length

        derivedImpl[A, elemTypes, label, labels](
          label,
          labels,
          labels.toSet,
          IArray.from(Helpers.typesOfTuple(TypeRepr.of[elemTypes], Nil).map(_.asType))
        )(
          using q,
          aType,
          new TypeLength[elemTypes] {
            type Length = lengthV.type
            override def length: Length = lengthV
          }
        )

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String, Labels <: Tuple](
      label: Label,
      namesArr: IArray[String],
      namesSet: Set[String],
      typesArr: IArray[Type[_]]
  )(
      using q: Quotes,
      aType: Type[A],
      typeLength: TypeLength[ElemTypes]
  ): ExprHKDProductGeneric[A] {
    type Gen[F[_]] = ProductK[F, ElemTypes]
    type Index[_]  = Finite[typeLength.Length]
    type TypeName  = Label
    type TupleRep  = ElemTypes
  } = new ExprHKDProductGeneric[A]:
    override type Gen[F[_]] = ProductK[F, ElemTypes]
    override type Index[_]  = Finite[typeLength.Length]
    override type ElemTop   = Helpers.TupleUnion[ElemTypes, Nothing]

    override def types: Gen[Type] = ProductK.ofProductUnsafe[Type, ElemTypes](ArrayProduct(typesArr))

    override type TypeName = Label

    override def typeName: TypeName = label

    opaque type Names <: String = String

    override def names: Gen[Const[Names]] =
      ProductK.ofProductUnsafe[Const[Names], ElemTypes](ArrayProduct(namesArr))

    override def stringToName(s: String): Option[Names] = Option.when(namesSet(s))(s)

    private lazy val nameMap =
      namesArr.zipWithIndex.toMap

    override def nameToIndex(name: Names): IdxWrapper[_ <: ElemTop] = IdxWrapper(
      Finite.unsafeApply(nameMap(name))
    )

    override type TupleRep = ElemTypes

    override def genToTuple[F[_]](gen: Gen[F]): Helpers.TupleMap[TupleRep, F] = gen.tuple

    override def tupleToGen[F[_]](tuple: Helpers.TupleMap[TupleRep, F]): Gen[F] = ProductK.ofTuple(tuple)

    override def catTo(a: Cat[A]): Gen[Cat] = tabulateK([X] => (idx: Index[X]) => a.productElementCat(idx))

    override def catFrom(a: Gen[Cat]): Cat[A] =
      import q.reflect.*
      val exprs  = (a.product.productIterator: Iterator[Any]).asInstanceOf[Iterator[Cat[_]]].toList
      val aClass = TypeRepr.of[A].classSymbol.getOrElse(report.errorAndAbort(s"Can't find class of ${Type.show[A]}"))
      Option.when(aClass.companionModule.exists)(aClass.companionModule).map { companionSymbol =>
        companionSymbol.methodMember("apply").filter { applySymbol =>
          val paramsSymbols = applySymbol.paramSymss

          ???
        }

        ???
      }

      aClass.caseFields

      ???

    override def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B =
      HKDGeneric.tabulateFoldLeftImpl(typeLength.length, start, f)

    override def tabulateTraverseK[G[_], B[_]](f: [X] => Finite[typeLength.Length] => G[B[X]])(
        using Applicative[G]
    ): G[Gen[B]] =
      HKDGeneric.tabulateTraverseKImpl(typeLength.length, f)

    override def tabulateTraverseKOption[B[_]](
        f: [X] => Finite[typeLength.Length] => Option[B[X]]
    ): Option[ProductK[B, ElemTypes]] = HKDGeneric.tabulateTraverseKOptionImpl(typeLength.length, f)

    override def tabulateTraverseKEither[E, B[_]](
        f: [X] => Finite[typeLength.Length] => Either[E, B[X]]
    ): Either[E, ProductK[B, ElemTypes]] = HKDGeneric.tabulateTraverseKEitherImpl(typeLength.length, f)

    extension (a: Cat[A])
      def productElementCat[X](index: Index[X]): Cat[X] =
        import q.reflect.*
        given Type[X] = (typesArr(index.value): Type[_]).asInstanceOf[Type[X]]
        Select.unique(a.asTerm, namesArr(index.value)).asExprOf[X]

    override def summonInstancesOpt[F[_]: Type]: Option[Gen[Compose2[Expr, F]]] =
      import cats.syntax.all._
      typesArr.toSeq
        .traverse { case '[t] =>
          Expr.summon[F[t]]
        }
        .map(instances => ProductK.ofProductUnsafe[Compose2[Expr, F], ElemTypes](ArrayProduct(IArray.from(instances))))

    private val instance: BoundedRepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
      ProductK.productKInstance[ElemTypes]

    override val representable: BoundedRepresentableKC.Aux[Gen, Index] = instance
    override val traverse: TraverseKC[Gen]                             = instance

trait ExprHKDSumGeneric[A] extends GenHKDSumGeneric[A] with ExprHKDGeneric[A]:
  type Cat[B] = Expr[Option[B]]

  /**
    * Returns the index of a value. Because of soundness, this method can not be
    * used if X = A. In that case, use [[indexOfA]] instead.
    */
  def indexOf[X <: ElemTop](x: X): Expr[Index[X]]

  /**
    * Same as [[indexOfA]] but also essentially casts the value to the unknown
    * type, allowing further operations on it that requires that it is a subtype
    * of A.
    */
  def indexOfACasting(a: Expr[A]): ExprHKDSumGeneric.IndexOfACasting[Index, ElemTop]

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
  def to(a: Expr[A]): Gen[Cat]

  override def catTo(a: Cat[A]): Gen[Cat]

  /**
    * Convert a value of the higher kinded representation to [[A]]. Will only
    * return Some if only one of the fields is Some and the rest is None.
    */
  def from(a: Gen[Cat]): Cat[A] = catFrom(a)

  def catFrom(a: Gen[Cat]): Cat[A]

  extension (a: A) def productElementCat[X](index: Index[X]): Cat[X]

object ExprHKDSumGeneric:
  trait IndexOfACasting[Index[_], ElemTop] {
    type X0 <: ElemTop
    val index: Expr[Index[X0]]
    val value: Expr[X0]
  }

  object IndexOfACasting {
    class IndexOfACastingImpl[Index[_], ElemTop, X1 <: ElemTop](
        val index: Expr[Index[X1]],
        val value: Expr[X1]
    ) extends IndexOfACasting[Index, ElemTop] {
      type X0 = X1
    }
  }
