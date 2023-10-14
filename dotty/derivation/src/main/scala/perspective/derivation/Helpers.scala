package perspective.derivation

import scala.annotation.tailrec
import scala.compiletime.*
import scala.quoted.*

object Helpers {

  inline def unsafeIArrayToArray[A](arr: IArray[A]): Array[A] = arr.asInstanceOf[Array[A]]

  inline def unsafeArrayToIArray[A](arr: Array[A]): IArray[A] = arr.asInstanceOf[IArray[A]]

  inline def boxAny(any: Any): AnyRef = any.asInstanceOf[AnyRef]

  /** A type returning if two types are equal as under =:=. */
  type Eq[A, B] <: Boolean = A =:= B match {
    case B =:= A => true
    case _       => false
  }

  type TupleUnionLub[T <: Tuple, Lub, Acc <: Lub] <: Lub = T match {
    case h *: t =>
      h match {
        case Lub => TupleUnionLub[t, Lub, Acc | (h & Lub)]
      }
    case EmptyTuple =>
      Acc
  }

  type TupleUnion[T <: Tuple, Acc] = T match {
    case h *: t     => TupleUnion[t, Acc | h]
    case EmptyTuple => Acc
  }

  // TODO: Expand this to 22
  // TODO: Check performance with this compared to just Tuple.Map
  type TupleMap[T <: Tuple, F[_]] <: Tuple = T match {
    case Tuple1[t1]                           => Tuple1[F[t1]]
    case (t1, t2)                             => (F[t1], F[t2])
    case (t1, t2, t3)                         => (F[t1], F[t2], F[t3])
    case (t1, t2, t3, t4)                     => (F[t1], F[t2], F[t3], F[t4])
    case (t1, t2, t3, t4, t5)                 => (F[t1], F[t2], F[t3], F[t4], F[t5])
    case (t1, t2, t3, t4, t5, t6)             => (F[t1], F[t2], F[t3], F[t4], F[t5], F[t6])
    case (t1, t2, t3, t4, t5, t6, t7)         => (F[t1], F[t2], F[t3], F[t4], F[t5], F[t6], F[t7])
    case (t1, t2, t3, t4, t5, t6, t7, t8)     => (F[t1], F[t2], F[t3], F[t4], F[t5], F[t6], F[t7], F[t8])
    case (t1, t2, t3, t4, t5, t6, t7, t8, t9) => (F[t1], F[t2], F[t3], F[t4], F[t5], F[t6], F[t7], F[t8], F[t9])
    case _                                    => Tuple.Map[T, F]
  }

  type TupleUnionMap[T <: Tuple, F[_], Acc] = T match {
    case h *: t     => TupleUnionMap[t, F, F[h] | Acc]
    case EmptyTuple => Acc
  }

  trait TupleBuilder[T <: Tuple]:
    def +=(a: Any): this.type

    def result: T
  object TupleBuilder:
    inline def mkFor[T <: Tuple]: TupleBuilder[T] =
      val size = constValue[Tuple.Size[T]]
      new TupleBuilderWithLub[T, TupleUnion[T, Nothing]](
        size,
        new Array[TupleUnion[T, Nothing]](size),
        0
      )

  class TupleBuilderWithLub[T <: Tuple, Lub](size: Int, values: Array[Lub], private var i: Int)
      extends TupleBuilder[T] {
    override def +=(a: Any): this.type =
      values(i) = a.asInstanceOf[Lub]
      i += 1
      this

    override def result: T =
      if i != size then throw new IllegalStateException("Added the wrong amount of values to the tuple builder")
      else (Tuple.fromArray(values): Tuple).asInstanceOf[T]
  }

  /** Show a dealiased version of a type. */
  inline def showType[T]: String = ${ showTypeImpl[T] }

  def showTypeImpl[T: Type](using q: Quotes): Expr[String] =
    import q.reflect.*
    Expr(TypeRepr.of[T].dealias.show)

  /** A optimized value of [[constValueTuple]]. */
  inline def constValueTupleOptimized[T <: Tuple]: T =
    ${ constValueTupleOptimizedImpl[T] }

  /**
    * A version of [[constValueTuple]] that instead returns a list of the
    * result.
    */
  inline def constValueTupleToList[T <: Tuple, Lub]: List[Lub] =
    ${ constValueTupleToListImpl[T, Lub] }

  /**
    * A version of [[constValueTuple]] that instead returns a set of the result.
    */
  inline def constValueTupleToSet[T <: Tuple, Lub]: Set[Lub] =
    ${ constValueTupleToSetImpl[T, Lub] }

  /**
    * A version of [[constValueTuple]] that instead returns an IArray of the
    * result.
    */
  inline def constValueTupleToIArray[T <: Tuple, Lub]: IArray[Lub] =
    ${ constValueTupleToIArrayImpl[T, Lub] }

  inline def constValueTupleLength[T <: Tuple]: Int =
    ${ constValueTupleLengthImpl[T] }

  /** A optimized value of [[summonAll]]. */
  inline def summonAllOptimized[T <: Tuple]: T =
    ${ summonAllOptimizedImpl[T] }

  /**
    * A version of [[summonAll]] that instead returns an IArray of the result.
    */
  inline def summonAllToIArray[T <: Tuple, F[_]]: IArray[TupleUnionMap[T, F, Nothing]] =
    ${ summonAllToIArrayImpl[T, F, TupleUnionMap[T, F, Nothing]] }

  /** A version of [[summonAllToIArray]] which returns an IArray[Object]. */
  inline def summonAllToObjectIArray[T <: Tuple, F[_]]: IArray[Object] =
    ${ summonAllToIArrayImpl[T, F, Object] }

  extension (e: Expr[Any])
    private def safeAsExprOf[B: Type](size: Int)(using Quotes): Expr[B] =
      if size > 22 then e.asInstanceOf[Expr[B]] else e.asExprOf[B]

  private def constValueTupleOptimizedImpl[T <: Tuple: Type](using q: Quotes): Expr[T] = {
    import q.reflect.*

    val values =
      valuesOfConstantTuple(TypeRepr.of[T], Nil)
        .getOrElse(report.errorAndAbort(s"${Type.show[T]} is not a constant tuple type"))

    Expr.ofTupleFromSeq(values).safeAsExprOf[T](values.length)
  }

  private def constValueTupleTo[T <: Tuple: Type, Lub: Type, B](ifEmpty: Expr[B], f: Expr[Seq[Lub]] => Expr[B])(
      using q: Quotes
  ): Expr[B] = {
    import q.reflect.*

    val values =
      valuesOfConstantTuple(TypeRepr.of[T], Nil)
        .getOrElse(report.errorAndAbort(s"${Type.show[T]} is not a constant tuple type"))

    if values.isEmpty then ifEmpty else f(Expr.ofSeq((values: List[Expr[Any]]).asInstanceOf[List[Expr[Lub]]]))
  }

  private def constValueTupleToListImpl[T <: Tuple: Type, Lub: Type](using q: Quotes): Expr[List[Lub]] = {
    import q.reflect.*
    constValueTupleTo[T, Lub, List[Lub]]('{ List.empty }, args => '{ List($args: _*) })
  }

  private def constValueTupleToSetImpl[T <: Tuple: Type, Lub: Type](using q: Quotes): Expr[Set[Lub]] = {
    import q.reflect.*
    constValueTupleTo[T, Lub, Set[Lub]]('{ Set.empty }, args => '{ Set($args: _*) })
  }

  private def constValueTupleToIArrayImpl[T <: Tuple: Type, Lub: Type](using q: Quotes): Expr[IArray[Lub]] = {
    import q.reflect.*
    constValueTupleTo[T, Lub, IArray[Lub]](
      '{ IArray.empty[Lub](using summonInline[scala.reflect.ClassTag[Lub]]) },
      args => '{ IArray($args: _*)(using summonInline[scala.reflect.ClassTag[Lub]]) }
    )
  }

  private def constValueTupleLengthImpl[T <: Tuple: Type](using q: Quotes): Expr[Int] = {
    import q.reflect.*
    Expr(typesOfTuple(TypeRepr.of[T], Nil).length)
  }

  private def summonAllOptimizedImpl[T <: Tuple: Type](using q: Quotes): Expr[T] = {
    import q.reflect.*

    val types = typesOfTuple(TypeRepr.of[T], Nil)
    Expr
      .ofTupleFromSeq(types.map { tpe =>
        tpe.asType match {
          case '[t] =>
            Expr.summon[t].getOrElse(report.errorAndAbort(s"Unable to to find implicit instance for ${tpe.show}"))
        }
      })
      .safeAsExprOf[T](types.length)
  }

  private def summonAllToIArrayImpl[T <: Tuple: Type, F[_]: Type, Res: Type](
      using q: Quotes
  ): Expr[IArray[Res]] = {
    import q.reflect.*

    val types = typesOfTuple(TypeRepr.of[T], Nil)
    val args = Varargs[Res](
      types.map { tpe =>
        tpe.asType match {
          case '[t] =>
            Expr
              .summon[F[t]]
              .getOrElse(report.errorAndAbort(s"Unable to to find implicit instance for ${tpe.show}"))
              .safeAsExprOf[Res](types.length)
        }
      }
    )

    '{ IArray($args: _*)(using summonInline[scala.reflect.ClassTag[Res]]) }
  }

  @tailrec
  private[derivation] def typesOfTuple(
      using q: Quotes
  )(tpe: q.reflect.TypeRepr, acc: List[q.reflect.TypeRepr]): List[q.reflect.TypeRepr] =
    import q.reflect.*
    val cons = Symbol.classSymbol("scala.*:")
    tpe.widenTermRefByName.dealias match
      case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
        tpes.reverse_:::(acc)
      case AppliedType(tp, List(headType, tailType)) if tp.derivesFrom(cons) =>
        typesOfTuple(tailType, headType :: acc)
      case tpe =>
        if tpe.derivesFrom(Symbol.classSymbol("scala.EmptyTuple")) then acc.reverse
        else report.errorAndAbort(s"Unknown type encountered in tuple ${tpe.show}")

  // Modified version of Type.valueOfTuple
  @tailrec
  private[derivation] def valuesOfConstantTuple(
      using q: Quotes
  )(tpe: q.reflect.TypeRepr, acc: List[Expr[Any]]): Option[List[Expr[Any]]] =
    import q.reflect.*
    val cons = Symbol.classSymbol("scala.*:")
    tpe.widenTermRefByName.dealias match
      case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
        tpes
          .foldRight(Option(Nil: List[Expr[Any]])) {
            case (_, None)               => None
            case (ValueOf(v), Some(acc)) => Some(v :: acc)
            case _                       => None
          }
          .map(foldAcc => foldAcc.reverse_:::(acc))
      case AppliedType(tp, List(ValueOf(headValue), tail)) if tp.derivesFrom(cons) =>
        valuesOfConstantTuple(tail, headValue :: acc)
      case tpe =>
        if tpe.derivesFrom(Symbol.classSymbol("scala.EmptyTuple")) then Some(acc.reverse)
        else None

  private object ValueOf:
    def unapply(using q: Quotes)(tpe: quotes.reflect.TypeRepr): Option[Expr[Any]] =
      import quotes.reflect.*
      tpe.widenTermRefByName.dealias match
        case ConstantType(BooleanConstant(const)) => Some(Expr(const))
        case ConstantType(ByteConstant(const))    => Some(Expr(const))
        case ConstantType(ShortConstant(const))   => Some(Expr(const))
        case ConstantType(IntConstant(const))     => Some(Expr(const))
        case ConstantType(LongConstant(const))    => Some(Expr(const))
        case ConstantType(FloatConstant(const))   => Some(Expr(const))
        case ConstantType(DoubleConstant(const))  => Some(Expr(const))
        case ConstantType(CharConstant(const))    => Some(Expr(const))
        case ConstantType(StringConstant(const))  => Some(Expr(const))
        case ConstantType(const)                  => report.errorAndAbort(s"Unknown constant ${const.show}")
        case _                                    => None
}
