package perspective.derivation

import scala.language.implicitConversions

import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

import cats.data.Validated
import cats.kernel.{BoundedEnumerable, Order}
import cats.{Applicative, FlatMap, Foldable, Functor, Monad}
import perspective.*

sealed trait ExprHKDGeneric[A] extends GenHKDGeneric[A]:

  def genType: Type[Gen]

  def types: Gen[Type]

  def summonInstancesOpt[F[_]: Type]: Option[Gen[Compose2[Expr, F]]]

  def summonInstances[F[_]: Type]: Gen[Compose2[Expr, F]]

  extension [B[_]: Type](gen: Gen[B]) def traverseKExprId[D[_]: Type](f: B :~>: Compose2[Expr, D]): Expr[Gen[D]]

  def tabulateTraverseKExprId[B[_]: Type](f: Index :~>: Compose2[Expr, B]): Expr[Gen[B]]

  def tabulateTraverseKExpr[B[_]: Type, D[_]: Type](
      f: Index :~>: Compose3[Expr, B, D],
      BAppExpr: Expr[Applicative[B]]
  ): Expr[B[Gen[D]]]

object ExprHKDGeneric:
  def tabulateTraverseKExprId[ElemTypes <: Tuple: Type, B[_]: Type](using typeLength: TypeLength[ElemTypes])(
      f: Finite[typeLength.Length] :#~>: Compose2[Expr, B]
  )(
      using q: Quotes
  ): Expr[ProductK[B, ElemTypes]] =
    val acc    = new collection.mutable.ArrayBuffer[Expr[Object]](typeLength.length)
    var i: Int = 0
    while (i < typeLength.length) {
      acc += '{
        ${ f[Object](Finite.unsafeApply(i)) }.asInstanceOf[Object]
      }
      i += 1
    }

    '{
      val objArr = Array(${ Expr.ofSeq(acc.toSeq) }*)
      ProductK.ofProductUnsafe(ArrayProduct.ofArrayUnsafe(objArr))
    }

  def tabulateTraverseKExpr[ElemTypes <: Tuple: Type, B[_], D[_]](using typeLength: TypeLength[ElemTypes])(
      f: Finite[typeLength.Length] :#~>: Compose3[Expr, B, D],
      BAppExpr: Expr[Applicative[B]]
  )(using q: Quotes, b: Type[B], d: Type[D]): Expr[B[ProductK[D, ElemTypes]]] =
    var acc: Expr[B[List[Object]]] = '{ $BAppExpr.pure(List.empty[Object]) }

    var i: Int = 0
    while (i < typeLength.length) {
      acc = '{ $BAppExpr.map2(${ f[Object](Finite.unsafeApply(i)) }, $acc)((v, a) => v.asInstanceOf[Object] :: a) }
      i += 1
    }

    '{
      $BAppExpr.map($acc) { a =>
        ProductK.ofProductUnsafe(ArrayProduct.ofArrayUnsafe(a.reverseIterator.toArray))
      }
    }

  def summonInstances[ElemTypes <: Tuple, F[_]: Type](typesArr: IArray[Type[_]])(
      using q: Quotes
  ): ProductK[Compose2[Expr, F], ElemTypes] =
    import cats.syntax.all._
    import q.reflect.*
    val instancesValidated = typesArr.toSeq.traverse { case '[t] =>
      Implicits.search(TypeRepr.of[F[t]]) match
        case iss: ImplicitSearchSuccess => iss.tree.asExprOf[F[t]].validNel
        case isf: ImplicitSearchFailure => isf.explanation.invalidNel
    }

    instancesValidated match
      case Validated.Valid(instances) =>
        ProductK.ofProductUnsafe[Compose2[Expr, F], ElemTypes](ArrayProduct(IArray.from(instances)))
      case Validated.Invalid(errors) =>
        errors.init.foreach(e => report.error(e))
        report.errorAndAbort(errors.last)

  def summonInstancesOpt[ElemTypes <: Tuple, F[_]: Type](typesArr: IArray[Type[_]])(
      using q: Quotes
  ): Option[ProductK[Compose2[Expr, F], ElemTypes]] =
    import cats.syntax.all._
    typesArr.toSeq
      .traverse { case '[t] =>
        Expr.summon[F[t]]
      }
      .map(instances => ProductK.ofProductUnsafe[Compose2[Expr, F], ElemTypes](ArrayProduct(IArray.from(instances))))

trait ExprHKDProductGeneric[A] extends GenHKDProductGeneric[A] with ExprHKDGeneric[A]:
  type Cat[B] = Expr[B]

  def idFrom(gen: Cat[Gen[Id]]): Cat[A]

  def idTo(a: Cat[A]): Cat[Gen[Id]]

  def tabulateFlatMappableExpr[B[_]: Type, D[_]: Type, R: Type](using q: Quotes)(
      f: Index :~>: Compose3[Expr, B, D],
      transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R],
      extractFlatMap: Quotes ?=> [X] => (Expr[B[D[X]]], Index[X], Quotes ?=> Expr[D[X]] => Expr[B[R]]) => Expr[B[R]],
      extractMap: Quotes ?=> [X] => (Expr[B[D[X]]], Index[X], Quotes ?=> Expr[D[X]] => Expr[R]) => Expr[B[R]]
  ): Expr[B[R]]

  def tabulateFlatMapExpr[B[_]: Type, D[_]: Type, R: Type](using q: Quotes)(
      f: Index :~>: Compose3[Expr, B, D],
      transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R],
      BFlatMapExpr: Expr[Monad[B]]
  ): Expr[B[R]]

  def tabulateMatchExprOption[D[_]: Type, R: Type](using q: Quotes)(
      f: Index :~>: Compose3[Expr, Option, D],
      transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R]
  ): Expr[Option[R]]

  def tabulateMatchExprEither[E: Type, D[_]: Type, R: Type](using q: Quotes)(
      f: Index :~>: Compose3[Expr, [X] =>> Either[E, X], D],
      transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R]
  ): Expr[Either[E, R]]

object ExprHKDProductGeneric:

  transparent inline given derived[A](using q: Quotes, aType: Type[A]): ExprHKDProductGeneric[A] =
    Expr.summon[Mirror.ProductOf[A]] match
      case Some('{
            type elemTypes <: Tuple
            type label <: String
            type labels <: Tuple
            $m: Mirror.ProductOf[A] {
              type MirroredElemTypes  = `elemTypes`
              type MirroredLabel      = `label`
              type MirroredElemLabels = `labels`
            }
          }) =>
        import q.reflect.*
        val labels  = Type.valueOfTuple[labels].get.toIArray.map(e => (e: Any).asInstanceOf[String])
        val label   = Type.valueOfConstant[label].get
        val lengthV = labels.length

        derivedImpl[A, elemTypes, label, labels](
          label,
          labels,
          labels.toSet,
          IArray.from(Helpers.typesOfTuple(TypeRepr.of[elemTypes], Nil).map(_.asType)),
          m
        )(
          using q,
          aType,
          summon[Type[label]],
          summon[Type[labels]],
          summon[Type[elemTypes]],
          new TypeLength[elemTypes] {
            type Length = lengthV.type
            override def length: Length = lengthV
          }
        )

      case _ =>
        q.reflect.report.errorAndAbort(s"Could not find mirror for ${Type.show[A]}")

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String, Labels <: Tuple](
      label: Label,
      namesArr: IArray[String],
      namesSet: Set[String],
      typesArr: IArray[Type[_]],
      m: Expr[
        Mirror.ProductOf[A] {
          type MirroredElemTypes  = ElemTypes
          type MirroredLabel      = Label
          type MirroredElemLabels = Labels
        }
      ]
  )(
      using q: Quotes,
      aType: Type[A],
      labelType: Type[Label],
      labelsType: Type[Labels],
      elemTypes: Type[ElemTypes],
      typeLength: TypeLength[ElemTypes]
  ): ExprHKDProductGeneric[A] {
    type Gen[F[_]] = ProductK[F, ElemTypes]
    type Index[_]  = Finite[typeLength.Length]
    type TypeName  = Label
    type TupleRep  = ElemTypes
  } = new ExprHKDProductGeneric[A]:
    override type Gen[F[_]] = ProductK[F, ElemTypes]
    override type Index[_]  = Finite[typeLength.Length]

    override def genType: Type[Gen] = Type.of[Gen]

    override def types: Gen[Type] = ProductK.ofProductUnsafe[Type, ElemTypes](ArrayProduct(typesArr))

    override type TypeName = Label

    override def typeName: TypeName = label

    opaque type Names <: String = String

    override def names: Gen[Const[Names]] =
      ProductK.ofProductUnsafe[Const[Names], ElemTypes](ArrayProduct(namesArr))

    override def stringToName(s: String): Option[Names] = Option.when(namesSet(s))(s)

    private lazy val nameMap = namesArr.zipWithIndex.toMap

    override def nameToIndex(name: Names): IdxWrapper[_ <: ElemTop] =
      HKDGeneric.IdxWrapper(Finite.unsafeApply(nameMap(name)))

    override type TupleRep = ElemTypes

    override def genToTuple[F[_]](gen: Gen[F]): Helpers.TupleMap[TupleRep, F] = gen.tuple

    override def tupleToGen[F[_]](tuple: Helpers.TupleMap[TupleRep, F]): Gen[F] = ProductK.ofTuple(tuple)

    override def idFrom(gen: Expr[Gen[Id]]): Expr[A] = '{ $m.fromProduct($gen.product) }

    override def idTo(a: Expr[A]): Expr[Gen[Id]] = '{ ProductK.ofProductUnsafe($a.asInstanceOf[Product]) }

    override def catTo(a: Cat[A]): Gen[Cat] = tabulateK([X] => (idx: Index[X]) => a.productElementCat(idx))

    override def catFrom(a: Gen[Cat]): Cat[A] =
      import q.reflect.*
      val exprs   = (a.product.productIterator: Iterator[Any]).asInstanceOf[Iterator[Cat[_]]].toList
      val aClass  = TypeRepr.of[A].classSymbol.getOrElse(report.errorAndAbort(s"Can't find class of ${Type.show[A]}"))
      val newExpr = Select(New(TypeTree.ref(aClass)), aClass.primaryConstructor)
      val types   = TypeRepr.of[A].typeArgs

      val (term, remainingArgs, remainingTypes) =
        aClass.primaryConstructor.paramSymss.foldLeft((newExpr: Term, exprs, types)) {
          case ((term, args, types), params) =>
            if (params.exists(_.isType))
              val (applyTypes, remainingTypes) = types.splitAt(params.length)
              (TypeApply(term, applyTypes.map(t => TypeTree.of(using t.asType))), args, remainingTypes)
            else
              val (applyArgs, remainingArgs) = args.splitAt(params.length)
              (Apply(term, applyArgs.map(_.asTerm)), remainingArgs, types)
        }
      if remainingArgs.nonEmpty then report.errorAndAbort(s"Unconsumed arguments for new $remainingArgs")
      if remainingTypes.nonEmpty then report.errorAndAbort(s"Unconsumed arguments for new $remainingTypes")

      term.asExprOf[A]

    extension [B[_]: Type](gen: Gen[B])
      override def traverseKExprId[D[_]: Type](f: B :~>: Compose2[Expr, D]): Expr[Gen[D]] =
        tabulateTraverseKExprId([X] => (idx: Index[X]) => f(gen.indexK(idx)): Compose2[Expr, D][X])

    override def tabulateTraverseKExprId[B[_]: Type](f: Index :~>: Compose2[Expr, B]): Expr[Gen[B]] =
      ExprHKDGeneric.tabulateTraverseKExprId(f)

    override def tabulateTraverseKExpr[B[_]: Type, D[_]: Type](
        f: Index :~>: Compose3[Expr, B, D],
        BAppExpr: Expr[Applicative[B]]
    ): Expr[B[Gen[D]]] =
      ExprHKDGeneric.tabulateTraverseKExpr(f, BAppExpr)

    override def tabulateFlatMappableExpr[B[_]: Type, D[_]: Type, R: Type](using q: Quotes)(
        f: Index :~>: Compose3[Expr, B, D],
        transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R],
        extractFlatMap: Quotes ?=> [X] => (Expr[B[D[X]]], Index[X], Quotes ?=> Expr[D[X]] => Expr[B[R]]) => Expr[B[R]],
        extractMap: Quotes ?=> [X] => (Expr[B[D[X]]], Index[X], Quotes ?=> Expr[D[X]] => Expr[R]) => Expr[B[R]]
    ): Expr[B[R]] =
      val ts = types

      def lastStep(using q: Quotes)(argsRev: List[Expr[Any]]): Expr[R] =
        import q.reflect.*
        val gen: Gen[Const[Expr[Any]]] =
          ProductK.ofProductUnsafe[Const[Expr[Any]], ElemTypes](
            ArrayProduct.ofArrayUnsafe(argsRev.reverseIterator.toArray)
          )

        transform(using q)(
          tabulateK[Compose2[Expr, D], Nothing] {
            [X] =>
              (idx: Index[X]) =>
                given Type[X] = ts.indexK(idx)
                gen.indexK(idx).asExprOf[D[X]]
          }
        )

      def rec[X](using q: Quotes)(idx: Index[X], argsRev: List[Expr[Any]]): Expr[B[R]] =
        val nextIdx = idx.value + 1
        if nextIdx >= typeLength.length then
          extractMap(using q)[X](
            f(idx),
            idx,
            newQ ?=>
              x =>
                import newQ.reflect.*
                lastStep(using newQ)(x :: argsRev)
          )
        else
          extractFlatMap(using q)[X](
            f(idx),
            idx,
            newQ ?=>
              x =>
                import newQ.reflect.*
                rec(using newQ)(Finite.unsafeApply(nextIdx), x :: argsRev)
          )

      rec(using q)(Finite.unsafeApply(0), Nil)

    override def tabulateFlatMapExpr[B[_]: Type, D[_]: Type, R: Type](using q: Quotes)(
        f: Index :~>: Compose3[Expr, B, D],
        transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R],
        BFlatMapExpr: Expr[Monad[B]]
    ): Expr[B[R]] =
      val ts = types
      tabulateFlatMappableExpr[B, D, R](using q)(
        f,
        transform,
        newQ ?=>
          [X] =>
            (ex: Expr[B[D[X]]], idx: Index[X], cont: Quotes ?=> Expr[D[X]] => Expr[B[R]]) =>
              import newQ.reflect.*
              val flatMapFunSym = TypeRepr.of[FlatMap[B]].typeSymbol.declaredMethod("flatMap").head
              val flatMapSelect = Select(BFlatMapExpr.asTerm, flatMapFunSym)
              given Type[X]     = ts.indexK(idx)

              val app = Apply(
                TypeApply(flatMapSelect, List(TypeTree.of[D[X]], TypeTree.of[R])),
                List(ex.asTerm)
              )

              Apply(
                app,
                List(
                  Lambda(
                    Symbol.spliceOwner,
                    MethodType(List(s"arg$idx"))(_ => List(TypeRepr.of[D[X]]), _ => TypeRepr.of[B[R]]),
                    {
                      case (newOwner, List(arg)) => cont(using newOwner.asQuotes)(arg.asExprOf[D[X]]).asTerm
                      case (_, _)                => sys.error("Got invalid args")
                    }
                  )
                )
              ).asExprOf[B[R]]
        ,
        newQ ?=>
          [X] =>
            (ex: Expr[B[D[X]]], idx: Index[X], cont: Quotes ?=> Expr[D[X]] => Expr[R]) =>
              import newQ.reflect.*
              val mapFunSym = TypeRepr.of[Functor[B]].typeSymbol.declaredMethod("map").head
              val mapSelect = Select(BFlatMapExpr.asTerm, mapFunSym)
              given Type[X] = ts.indexK(idx)

              val app = Apply(
                TypeApply(mapSelect, List(TypeTree.of[D[X]], TypeTree.of[R])),
                List(ex.asTerm)
              )

              Apply(
                app,
                List(
                  Lambda(
                    Symbol.spliceOwner,
                    MethodType(List(s"arg$idx"))(_ => List(TypeRepr.of[D[X]]), _ => TypeRepr.of[R]),
                    {
                      case (newOwner, List(arg)) => cont(using newOwner.asQuotes)(arg.asExprOf[D[X]]).asTerm
                      case (_, _)                => sys.error("Got invalid args")
                    }
                  )
                )
              ).asExprOf[B[R]]
      )

    override def tabulateMatchExprOption[D[_]: Type, R: Type](using q: Quotes)(
        f: Index :~>: Compose3[Expr, Option, D],
        transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R]
    ): Expr[Option[R]] =
      val ts = types
      tabulateFlatMappableExpr(using q)(
        f,
        transform,
        newQ ?=>
          [X] =>
            (ex: Expr[Option[D[X]]], idx: Index[X], cont: Quotes ?=> Expr[D[X]] => Expr[Option[R]]) =>
              given Type[X] = ts.indexK(idx)
              import newQ.reflect.*

              ValDef
                .let(Symbol.spliceOwner, s"arg${idx.value}", ex.asTerm) { vTerm =>
                  val v = vTerm.asExprOf[Option[D[X]]]
                  '{
                    // Simpler bytecode
                    if $v != None then ${ cont(using newQ)('{ $v.asInstanceOf[Some[D[X]]].value }) }
                    else None
                  }.asTerm
                }
                .asExprOf[Option[R]]
        ,
        newQ ?=>
          [X] =>
            (ex: Expr[Option[D[X]]], idx: Index[X], cont: Quotes ?=> Expr[D[X]] => Expr[R]) =>
              given Type[X] = ts.indexK(idx)
              import newQ.reflect.*

              ValDef
                .let(Symbol.spliceOwner, s"arg${idx.value}", ex.asTerm) { vTerm =>
                  val v = vTerm.asExprOf[Option[D[X]]]
                  '{
                    // Simpler bytecode
                    if $v != None then Some(${ cont(using newQ)('{ $v.asInstanceOf[Some[D[X]]].value }) })
                    else None
                  }.asTerm
                }
                .asExprOf[Option[R]]
      )

    override def tabulateMatchExprEither[E: Type, D[_]: Type, R: Type](using q: Quotes)(
        f: Index :~>: Compose3[Expr, [X] =>> Either[E, X], D],
        transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R]
    ): Expr[Either[E, R]] =
      val ts = types
      tabulateFlatMappableExpr(using q)(
        f,
        transform,
        newQ ?=>
          [X] =>
            (ex: Expr[Either[E, D[X]]], idx: Index[X], cont: Quotes ?=> Expr[D[X]] => Expr[Either[E, R]]) =>
              given Type[X] = ts.indexK(idx)
              import newQ.reflect.*

              ValDef
                .let(Symbol.spliceOwner, s"arg${idx.value}", ex.asTerm) { vTerm =>
                  val v = vTerm.asExprOf[Either[E, D[X]]]
                  '{
                    // Simpler bytecode
                    if $v.isInstanceOf[Right[_, _]] then
                      ${ cont(using newQ)('{ $v.asInstanceOf[Right[E, D[X]]].value }) }
                    else $v.asInstanceOf[Either[E, R]]
                  }.asTerm
                }
                .asExprOf[Either[E, R]]
        ,
        newQ ?=>
          [X] =>
            (ex: Expr[Either[E, D[X]]], idx: Index[X], cont: Quotes ?=> Expr[D[X]] => Expr[R]) =>
              given Type[X] = ts.indexK(idx)
              import newQ.reflect.*

              ValDef
                .let(Symbol.spliceOwner, s"arg${idx.value}", ex.asTerm) { vTerm =>
                  val v = vTerm.asExprOf[Either[E, D[X]]]
                  '{
                    // Simpler bytecode
                    if $v.isInstanceOf[Right[_, _]] then
                      Right(${ cont(using newQ)('{ $v.asInstanceOf[Right[E, D[X]]].value }) })
                    else $v.asInstanceOf[Either[E, R]]
                  }.asTerm
                }
                .asExprOf[Either[E, R]]
      )

    override def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B =
      HKDGeneric.tabulateFoldLeftImpl(typeLength.length, start, f)

    override def tabulateTraverseK[G[_]: Applicative, B[_]](f: Index :~>: Compose2[G, B]): G[Gen[B]] =
      HKDGeneric.tabulateTraverseKImpl(typeLength.length, f)

    override def tabulateTraverseKOption[B[_]](
        f: Index :~>: Compose2[Option, B]
    ): Option[Gen[B]] = HKDGeneric.tabulateTraverseKOptionImpl(typeLength.length, f)

    override def tabulateTraverseKEither[E, B[_]](
        f: Index :~>: Compose2[[X] =>> Either[E, X], B]
    ): Either[E, Gen[B]] = HKDGeneric.tabulateTraverseKEitherImpl(typeLength.length, f)

    extension (a: Cat[A])
      def productElementCat[X](index: Index[X]): Cat[X] =
        import q.reflect.*
        given Type[X] = (typesArr(index.value): Type[_]).asInstanceOf[Type[X]]
        Select.unique(a.asTerm, namesArr(index.value)).asExprOf[X]

    override def summonInstances[F[_]: Type]: Gen[Compose2[Expr, F]] =
      ExprHKDGeneric.summonInstances(typesArr)

    override def summonInstancesOpt[F[_]: Type]: Option[Gen[Compose2[Expr, F]]] =
      ExprHKDGeneric.summonInstancesOpt(typesArr)

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
  def indexOf[X <: ElemTop: Type](x: Expr[X]): Expr[Index[X]]

  /** Same as [[indexOf]] but also works for values of type A. */
  def indexOfA(a: Expr[A]): Expr[IdxWrapper[_ <: ElemTop]]

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

  extension (a: Cat[A]) def productElementCat[X](index: Index[X]): Cat[X]

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

  transparent inline given derived[A](using q: Quotes, aType: Type[A]): ExprHKDSumGeneric[A] =
    Expr.summon[Mirror.SumOf[A]] match
      case Some('{
            type elemTypes <: Tuple
            type label <: String
            type labels <: Tuple
            $m: Mirror.SumOf[A] {
              type MirroredElemTypes  = `elemTypes`
              type MirroredLabel      = `label`
              type MirroredElemLabels = `labels`
            }
          }) =>
        import q.reflect.*
        val labels  = Type.valueOfTuple[labels].get.toIArray.map(e => (e: Any).asInstanceOf[String])
        val label   = Type.valueOfConstant[label].get
        val lengthV = labels.length

        derivedImpl[A, elemTypes, label, labels](
          label,
          labels,
          labels.toSet,
          IArray.from(Helpers.typesOfTuple(TypeRepr.of[elemTypes], Nil).map(_.asType)),
          m
        )(
          using q,
          aType,
          summon[Type[label]],
          summon[Type[labels]],
          summon[Type[elemTypes]],
          new TypeLength[elemTypes] {
            type Length = lengthV.type

            override def length: Length = lengthV
          },
          summon[Type[lengthV.type]]
        )

      case _ =>
        q.reflect.report.errorAndAbort(s"Could not find mirror for ${Type.show[A]}")

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String, Labels <: Tuple](
      label: Label,
      namesArr: IArray[String],
      namesSet: Set[String],
      typesArr: IArray[Type[_]],
      m: Expr[
        Mirror.SumOf[A] {
          type MirroredElemTypes  = ElemTypes
          type MirroredLabel      = Label
          type MirroredElemLabels = Labels
        }
      ]
  )(
      using q: Quotes,
      aType: Type[A],
      labelType: Type[Label],
      labelsType: Type[Labels],
      elemTypes: Type[ElemTypes],
      typeLength: TypeLength[ElemTypes],
      typeLengthType: Type[typeLength.Length]
  ): ExprHKDSumGeneric[A] {
    type Gen[F[_]] = ProductK[F, ElemTypes]
    type Index[_]  = Finite[typeLength.Length]
    type TypeName  = Label
    type TupleRep  = ElemTypes
    type ElemTop   = A & Helpers.TupleUnion[TupleRep, Nothing]
  } = new ExprHKDSumGeneric[A]:
    override type Gen[F[_]] = ProductK[F, ElemTypes]
    override type Index[_]  = Finite[typeLength.Length]
    override type ElemTop = A & Helpers.TupleUnion[TupleRep, Nothing]

    override def genType: Type[Gen] = Type.of[Gen]

    override def types: Gen[Type] = ProductK.ofProductUnsafe[Type, ElemTypes](ArrayProduct(typesArr))

    override type TypeName = Label

    override def typeName: TypeName = label

    opaque type Names <: String = String

    override def names: Gen[Const[Names]] =
      ProductK.ofProductUnsafe[Const[Names], ElemTypes](ArrayProduct(namesArr))

    override def stringToName(s: String): Option[Names] = Option.when(namesSet(s))(s)

    private lazy val nameMap = namesArr.zipWithIndex.toMap

    override def nameToIndex(name: Names): IdxWrapper[_ <: ElemTop] =
      HKDGeneric.IdxWrapper(Finite.unsafeApply(nameMap(name)))

    override type TupleRep = ElemTypes

    override def genToTuple[F[_]](gen: Gen[F]): Helpers.TupleMap[TupleRep, F] = gen.tuple

    override def tupleToGen[F[_]](tuple: Helpers.TupleMap[TupleRep, F]): Gen[F] = ProductK.ofTuple(tuple)

    override def to(a: Expr[A]): Gen[Cat] = catTo('{ Some($a) })

    override def catTo(a: Cat[A]): Gen[Cat] =
      val ts = types
      representable.tabulateK {
        [Z] =>
          (i: Index[Z]) =>
            given Type[Z] = ts.indexK(i)
            '{
              val ao = $a
              ao.fold(None: Option[Z]) { a =>
                // This cast is safe as we know A = Z
                if ${ Expr(i.value) } == $m.ordinal(a) then Some((a: A).asInstanceOf[Z]) else None
              }
          }
      }

    override def catFrom(a: Gen[Cat]): Cat[A] =
      val ts = types
      val ret = tabulateFoldLeft('{ (false, None: Option[A]) }) { acc =>
        [X] =>
          (idx: Index[X]) =>
            given Type[X] = ts.indexK(idx)
            '{
              val (moreThanOne, v) = $acc
              if moreThanOne then (true, None: Option[A])
              else
                val v2 = ${ a.indexK(idx) }
                if v.isDefined && v2.isDefined then (true, None)
                else (false, v.orElse(v2.asInstanceOf[Option[A]]))
          }
      }
      '{ $ret._2 }


    override def indexOf[X <: ElemTop: Type](x: Expr[X]): Expr[Index[X]] =
      '{ Finite.unsafeApply($m.ordinal($x)) }

    override def indexOfA(a: Expr[A]): Expr[IdxWrapper[_ <: ElemTop]] =
      '{ new HKDGeneric.IdxWrapper[Index, ElemTop](Finite.unsafeApply($m.ordinal($a))) }

    override def indexOfACasting(a: Expr[A]): IndexOfACasting[Index, ElemTop] =
      val idx = '{ Finite.unsafeApply[typeLength.Length]($m.ordinal($a)) }
      new IndexOfACasting.IndexOfACastingImpl[Index, ElemTop, ElemTop](idx, a.asInstanceOf[Expr[ElemTop]])

    extension [B[_]: Type](gen: Gen[B])
      override def traverseKExprId[D[_]: Type](f: B :~>: Compose2[Expr, D]): Expr[Gen[D]] =
        tabulateTraverseKExprId([X] => (idx: Index[X]) => f(gen.indexK(idx)): Compose2[Expr, D][X])

    override def tabulateTraverseKExprId[B[_]: Type](f: Index :~>: Compose2[Expr, B]): Expr[Gen[B]] =
      ExprHKDGeneric.tabulateTraverseKExprId(f)

    override def tabulateTraverseKExpr[B[_]: Type, D[_]: Type](
        f: Index :~>: Compose3[Expr, B, D],
        BAppExpr: Expr[Applicative[B]]
    ): Expr[B[Gen[D]]] =
      ExprHKDGeneric.tabulateTraverseKExpr(f, BAppExpr)

    override def tabulateFoldLeft[B](start: B)(f: B => [X] => Index[X] => B): B =
      HKDGeneric.tabulateFoldLeftImpl(typeLength.length, start, f)

    override def tabulateTraverseK[G[_]: Applicative, B[_]](f: Index :~>: Compose2[G, B]): G[Gen[B]] =
      HKDGeneric.tabulateTraverseKImpl(typeLength.length, f)

    override def tabulateTraverseKOption[B[_]](
        f: Index :~>: Compose2[Option, B]
    ): Option[Gen[B]] = HKDGeneric.tabulateTraverseKOptionImpl(typeLength.length, f)

    override def tabulateTraverseKEither[E, B[_]](
        f: Index :~>: Compose2[[X] =>> Either[E, X], B]
    ): Either[E, Gen[B]] = HKDGeneric.tabulateTraverseKEitherImpl(typeLength.length, f)

    extension (a: Cat[A])
      override def productElementCat[X](index: Index[X]): Cat[X] =
        given Type[X] = types.indexK(index)
        '{
          val v = $a
          if v != None && $m.ordinal(v.asInstanceOf[Some[A]].value) == ${ Expr(index.value) } then
            v.asInstanceOf[Option[X]]
          else None
        }

    override def summonInstances[F[_]: Type]: Gen[Compose2[Expr, F]] =
      ExprHKDGeneric.summonInstances(typesArr)

    override def summonInstancesOpt[F[_]: Type]: Option[Gen[Compose2[Expr, F]]] =
      ExprHKDGeneric.summonInstancesOpt(typesArr)

    private val instance: BoundedRepresentableKC.Aux[Gen, Index] & TraverseKC[Gen] =
      ProductK.productKInstance[ElemTypes]

    override val representable: BoundedRepresentableKC.Aux[Gen, Index] = instance
    override val traverse: TraverseKC[Gen]                             = instance