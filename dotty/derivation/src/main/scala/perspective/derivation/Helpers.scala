package perspective.derivation

import scala.annotation.tailrec
import scala.compiletime.*
import scala.quoted.*

object Helpers {

  type Eq[A, B] <: Boolean = A =:= B match {
    case B =:= A => true
    case _       => false
  }

  inline def constValueTupleOptimized[T <: Tuple]: T =
    ${ constValueTupleOptimizedImpl[T] }

  inline def constValueTupleToList[T <: Tuple]: List[Tuple.Union[T]] =
    ${ constValueTupleToListImpl[T] }

  inline def constValueTupleToSet[T <: Tuple]: Set[Tuple.Union[T]] =
    ${ constValueTupleToSetImpl[T] }

  inline def constValueTupleToIArray[T <: Tuple]: IArray[Tuple.Union[T]] =
    ${ constValueTupleToIArrayImpl[T] }

  inline def summonAllOptimized[T <: Tuple]: T =
    ${ summonAllOptimizedImpl[T] }

  inline def summonAllToIArray[T <: Tuple]: IArray[Tuple.Union[T]] =
    ${ summonAllToIArrayImpl[T] }

  def constValueTupleOptimizedImpl[T <: Tuple: Type](using q: Quotes): Expr[T] = {
    import q.reflect.*

    val values =
      valuesOfConstantTuple(TypeRepr.of[T], Nil)
        .getOrElse(report.errorAndAbort(s"${Type.show[T]} is not a constant tuple type"))

    Expr.ofTupleFromSeq(values).asInstanceOf[Expr[T]]
  }

  private def constValueTupleTo[T <: Tuple: Type, B](ifEmpty: Expr[B], f: Expr[Seq[Tuple.Union[T]]] => Expr[B])(
      using q: Quotes
  ): Expr[B] = {
    import q.reflect.*

    val values =
      valuesOfConstantTuple(TypeRepr.of[T], Nil)
        .getOrElse(report.errorAndAbort(s"${Type.show[T]} is not a constant tuple type"))

    if values.isEmpty then ifEmpty else f(Expr.ofSeq(values.asInstanceOf[List[Expr[Tuple.Union[T]]]]))
  }

  def constValueTupleToListImpl[T <: Tuple: Type](using q: Quotes): Expr[List[Tuple.Union[T]]] = {
    import q.reflect.*
    constValueTupleTo[T, List[Tuple.Union[T]]]('{ List.empty }, args => '{ List($args: _*) })
  }

  def constValueTupleToSetImpl[T <: Tuple: Type](using q: Quotes): Expr[Set[Tuple.Union[T]]] = {
    import q.reflect.*
    constValueTupleTo[T, Set[Tuple.Union[T]]]('{ Set.empty }, args => '{ Set($args: _*) })
  }

  def constValueTupleToIArrayImpl[T <: Tuple: Type](using q: Quotes): Expr[IArray[Tuple.Union[T]]] = {
    import q.reflect.*
    constValueTupleTo[T, IArray[Tuple.Union[T]]](
      '{ IArray.empty[Tuple.Union[T]](using summonInline[scala.reflect.ClassTag[Tuple.Union[T]]]) },
      args => '{ IArray($args: _*)(using summonInline[scala.reflect.ClassTag[Tuple.Union[T]]]) }
    )
  }

  def summonAllOptimizedImpl[T <: Tuple: Type](using q: Quotes): Expr[T] = {
    import q.reflect.*

    Expr
      .ofTupleFromSeq(typesOfTuple(TypeRepr.of[T], Nil).map { tpe =>
        tpe.asType match {
          case '[t] =>
            Expr.summon[t].getOrElse(report.errorAndAbort(s"Unable to to find implicit instance for ${tpe.show}"))
        }
      })
      .asInstanceOf[Expr[T]]
  }

  def summonAllToIArrayImpl[T <: Tuple: Type](using q: Quotes): Expr[IArray[Tuple.Union[T]]] = {
    import q.reflect.*

    val args = Varargs[Tuple.Union[T]](
      typesOfTuple(TypeRepr.of[T], Nil).map { tpe =>
        tpe.asType match {
          case '[t] =>
            Expr
              .summon[t]
              .getOrElse(report.errorAndAbort(s"Unable to to find implicit instance for ${tpe.show}"))
              .asInstanceOf[Expr[Tuple.Union[T]]]
        }
      }
    )

    '{ IArray($args: _*)(using summonInline[scala.reflect.ClassTag[Tuple.Union[T]]]) }
  }

  @tailrec
  def typesOfTuple(using q: Quotes)(tpe: q.reflect.TypeRepr, acc: List[q.reflect.TypeRepr]): List[q.reflect.TypeRepr] =
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
  private def valuesOfConstantTuple(
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
