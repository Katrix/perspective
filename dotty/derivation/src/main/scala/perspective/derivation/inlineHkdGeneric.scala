package perspective.derivation

import scala.language.implicitConversions

import scala.annotation.{compileTimeOnly, tailrec}
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import scala.reflect.ClassTag

import cats.syntax.all.*
import cats.{Applicative, Functor, Monoid}
import perspective.*

/**
  * A type like [[HKDGeneric]] but where as much as possible is defined inline,
  * and tries to generate as simple bytecode as possible. Might be more error
  * prone, and produce bigger classes using this, but the bytecode is much
  * simpler.
  *
  * @tparam A
  *   The type being abstracted over.
  */
//noinspection ScalaUnusedSymbol
sealed trait InlineHKDGeneric[A]:
  /** A representation of [[A]] supporting higher kinded types. */
  type Gen[_[_]]

  /** The top type for the inner type of [[Index]] and [[Gen]]. */
  type ElemTop

  /** The index of the [[Gen]] type. */
  type Index <: Any { type X <: ElemTop }
  type IndexAux[X0 <: ElemTop] = Index { type X = X0 }

  /** A wrapper for [[Index]] where we want no bound on what [[X]] can be. */
  class IdxWrapper[X](val idx: IndexAux[X & ElemTop])

  given [X]: Conversion[IdxWrapper[X], IndexAux[X & ElemTop]] = _.idx

  given [X]: Conversion[IndexAux[X & ElemTop], IdxWrapper[X]] = new IdxWrapper(_)

  /** The name of the [[A]] type. */
  type TypeName <: String

  /** The name of the [[A]] type. */
  inline def typeName: TypeName

  /**
    * The name of the fields of [[A]] type. Field in this case can mean either
    * the children of a sum type, or the fields of a product type.
    */
  type Names <: String

  /**
    * The name of the fields of [[A]] type. Field in this case can mean either
    * the children of a sum type, or the fields of a product type.
    */
  inline def names: Gen[Const[Names]]

  /** Validates a string as a name if it matches the name of a field. */
  inline def stringToName(s: String): Option[Names]

  /** Returns the type of a field given its name. */
  type FieldOf[Name <: Names] <: ElemTop

  /** Returns the index of the field a name corresponds to. */
  inline def nameToIndex[Name <: Names](name: Name): IndexAux[FieldOf[Name]]

  /** A tuple representation of [[A]]. */
  type TupleRep <: Tuple

  /** Converts [[Gen]] to the tuple representation. */
  inline def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F]

  /** Converts the tuple representation to Gen. */
  inline def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F]

  inline given summonInstances[F[_]]: Gen[F]

  val idClassTag: ClassTag[ElemTop]
  given ClassTag[ElemTop] = idClassTag

  //
  // Typeclass ops
  //

  // Functor
  extension [A[_]](fa: Gen[A])
    inline def mapK[B[_]](inline f: A ~>: B): Gen[B]

    inline def mapConst[B](inline f: A ~>#: B): Gen[Const[B]] =
      mapK(f)

    inline def voidK: Gen[Const[Unit]] = asK(ValueK.const(()))

    inline def asK[B[_]](inline b: ValueK[B]): Gen[B] =
      mapK([Z] => (_: A[Z]) => b[Z]())

    inline def widen[B[D] >: A[D]]: Gen[B] = fa.asInstanceOf[Gen[B]]

  extension [A[_], B[_]](f: A ~>: B) inline def liftK: Gen[A] #~>#: Gen[B] = [C] => (fa: Gen[A]) => fa.mapK(f)

  // Apply
  extension [A[_], B[_]](ff: Gen[[D] =>> A[D] => B[D]])
    inline def ap(fa: Gen[A]): Gen[B] =
      ff.map2K(fa)([Z] => (f: A[Z] => B[Z], a: A[Z]) => f(a))

  extension [A[_]](fa: Gen[A])
    inline def map2K[B[_], Z[_]](fb: Gen[B])(inline f: [X] => (A[X], B[X]) => Z[X]): Gen[Z]

    inline def map2Const[B[_], Z](fb: Gen[B])(inline f: [X] => (A[X], B[X]) => Z): Gen[Const[Z]] =
      fa.map2K(fb)(f)

    inline def tupledK[B[_]](fb: Gen[B]): Gen[Tuple2K[A, B]] =
      fa.map2K(fb)([Z] => (a: A[Z], b: B[Z]) => (a, b))

  // Applicative
  extension [A[_]](a: ValueK[A]) inline def pure: Gen[A]

  inline def unitK: Gen[Const[Unit]] = ValueK.const(()).pure

  // Foldable
  extension [A[_]](fa: Gen[A])
    inline def foldLeftK[B](inline b: B)(inline f: B => A ~>#: B): B

    inline def foldMapK[B](inline f: A ~>#: B)(using B: Monoid[B]): B

  extension [A](fa: Gen[Const[A]]) inline def toListK: List[A]

  // Traverse
  extension [A[_]](fa: Gen[A])
    inline def traverseK[G[_], B[_]](
        inline f: A ~>: Compose2[G, B]
    )(using inline G: Applicative[G], classTag: ClassTag[B[ElemTop]]): G[Gen[B]]

    inline def traverseIdK[G[_]](inline f: A ~>: G)(using inline G: Applicative[G]): G[Gen[Id]] =
      traverseK(f)

    inline def sequenceIdK(using inline G: Applicative[A]): A[Gen[Id]] =
      fa.sequenceK

  extension [G[_], A[_]](fga: Gen[Compose2[G, A]]) inline def sequenceK(using inline G: Applicative[G]): G[Gen[A]]

  // Distributive
  extension [G[_]: Functor, A[_]](gfa: G[Gen[A]])
    inline def distributeK[B[_]](inline f: Compose2[G, A] ~>: B): Gen[B] =
      gfa.cosequenceK.mapK(f)

    inline def distributeConst[B](inline f: Compose2[G, A] ~>#: B): Gen[Const[B]] =
      distributeK[Const[B]](f)

    inline def cosequenceK: Gen[Compose2[G, A]]

  extension [G[_]: Functor, A](ga: G[A])
    inline def collectK[B[_]](inline f: A => Gen[B]): Gen[Compose2[G, B]] =
      ga.map(f).cosequenceK

  // Monad
  extension [A[_]](ffa: Gen[Const[Gen[A]]]) inline def flattenK: Gen[A] = ffa.flatMapK(FunctionK.identity)

  extension [A[_]](fa: Gen[A]) inline def flatMapK[B[_]](inline f: A ~>#: Gen[B]): Gen[B]

  // Representable
  inline def tabulateK[B[_]](inline f: (i: Index) => B[i.X], inline unrolling: Boolean = false)(
      using classTag: ClassTag[B[ElemTop]]
  ): Gen[B]

  inline def tabulateConst[B](inline f: Index => B)(using ClassTag[B]): Gen[Const[B]] = tabulateK(f)

  inline def tabulateFoldLeft[B](inline start: B, inline unrolling: Boolean = false)(inline f: (B, Index) => B): B

  inline def tabulateTraverseK[G[_], B[_]](
      inline f: (i: Index) => G[B[i.X]],
      inline unrolling: Boolean = false
  )(using inline G: Applicative[G], classTag: ClassTag[B[ElemTop]]): G[Gen[B]]

  inline def tabulateTraverseIdK[G[_]](inline f: (i: Index) => G[i.X])(using inline G: Applicative[G]): G[Gen[Id]] =
    tabulateTraverseK(f)

  inline def indicesK: Gen[IdxWrapper] = tabulateK(index => IdxWrapper(index))

  extension [B[_]](fa: Gen[B]) inline def indexK(rep: Index): B[rep.X]

//noinspection ScalaUnusedSymbol,DuplicatedCode
object InlineHKDGeneric:
  type Aux[A, Gen0[_[_]]] = InlineHKDGeneric[A] {
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

  transparent inline given derived[A](
      using m0: Mirror.Of[A]
  ): InlineHKDGeneric[A] =
    inline m0 match
      case m: Mirror.ProductOf[A] =>
        InlineHKDProductGeneric.derived[A](using m, summonInline[ClassTag[Tuple.Union[m.MirroredElemTypes]]])
      case m: Mirror.SumOf[A] =>
        InlineHKDSumGeneric
          .derived[A](using m, summonInline[ClassTag[InlineHKDGeneric.TupleUnionLub[m.MirroredElemTypes, A, Nothing]]])
  end derived

  extension [A](arr: IArray[A])
    private inline def quickArrayIndex(i: Int): A =
      arr.asInstanceOf[Array[A]](i)

  @compileTimeOnly("lateInlineMatch must be used within a supported InlineHKDGeneric method")
  def lateInlineMatch[A](a: A): A = sys.error("impossible")

  @compileTimeOnly("productElementIdExact must be used within a supported InlineHKDGeneric method")
  def productElementIdExact[A, ElemTop](a: A, idx: Int { type X <: ElemTop }): idx.X = sys.error("impossible")

  extension [A](arr: Array[A]) private[derivation] inline def asIArray: IArray[A] = arr.asInstanceOf[IArray[A]]

  private[derivation] inline def iteratorToArray[A[_], ElemTop](inline itIn: Iterator[A[ElemTop]], inline size: Int)(
      using ClassTag[A[ElemTop]]
  ): Array[A[ElemTop]] = {
    val it                     = itIn
    val arr: Array[A[ElemTop]] = new Array[A[ElemTop]](size)
    var i: Int                 = 0
    while (it.hasNext) {
      arr(i) = it.next()
      i += 1
    }
    arr
  }

  private class ProductElementIdExactExpander[Q <: Quotes, Types <: Tuple: Type, Fields <: Tuple: Type](using val q: Q)
      extends q.reflect.TreeMap {
    import q.reflect.*

    override def transformTerm(tree: Term)(owner: Symbol): Term =
      try {
        tree.asExpr match {
          case '{ InlineHKDGeneric.productElementIdExact[a2, elemTop]($a, $idx) } =>
            transformExact(a, idx)
          case _ =>
            super.transformTerm(tree)(owner)
        }
      } catch {
        case e: Exception =>
          // Tried to convert partially applied type to Expr. Ignoring it
          tree
      }

    def transformExact[A](a: Expr[A], idxExpr: Expr[Int]): Term = {
      def findConstantIdx(tpe: TypeRepr): Option[Int] = tpe match {
        case AndType(a, b)                => findConstantIdx(a).orElse(findConstantIdx(b))
        case ConstantType(IntConstant(i)) => Some(i)
        case Refinement(a, _, _)          => findConstantIdx(a)
        case t                            => None
      }

      val idx = findConstantIdx(idxExpr.asTerm.tpe.widenTermRefByName).getOrElse(idxExpr.valueOrAbort)

      val tpe = Helpers.typesOfTuple(TypeRepr.of[Types], Nil)(idx)
      val field = Helpers.valuesOfConstantTuple(TypeRepr.of[Fields], Nil) match {
        case Some(seq) => seq(idx).asInstanceOf[Expr[String]].valueOrAbort
        case None      => report.errorAndAbort("productElementIdExactImpl1 called with non constant fields type")
      }

      tpe.asType match {
        case '[t] => Select.unique(a.asTerm, field)
      }
    }
  }

  private class RefReplacer[Q <: Quotes](using val q: Q)(oldRef: q.reflect.Symbol, newRef: q.reflect.Ref)
      extends q.reflect.TreeMap {
    import q.reflect.*

    override def transformTerm(tree: Term)(owner: Symbol): Term =
      tree match {
        case Ident(id) if id == oldRef.name => newRef
        case _                              => super.transformTerm(tree)(owner)
      }
  }

  private class LateInlineMatchExpander[Q <: Quotes]()(using val q: Q) extends q.reflect.TreeMap {
    import q.reflect.*

    override def transformTerm(tree: Term)(owner: Symbol): Term = {
      tree.asExpr match {
        case '{ InlineHKDGeneric.lateInlineMatch[a]($a) } =>
          transformMatch(a, owner)
        case _ =>
          val res =
            try {
              super.transformTerm(tree)(owner)
            } catch {
              case _: Exception =>
                // FIXME: Have no idea why this happens. Just ignoring it for now.
                tree
            }

          res
      }
    }

    def transformMatch[A: Type](aExpr: Expr[A], owner: Symbol): Term = aExpr.asTerm match {
      case m @ Match(scrutinee, cases) =>
        val tpe = scrutinee.tpe.widenTermRefByName

        cases.foreach {
          case CaseDef(_, Some(_), _)                     => report.errorAndAbort("Cases in match can not have guards")
          case CaseDef(Bind(_, Typed(Ident(_), _)), _, _) =>
          case CaseDef(Bind(_, Ident(_)), _, _)           =>
          case caseDef => report.errorAndAbort("Invalid case in match inside lateInlineMatch", caseDef.pos)
        }

        val hasDefaultCase = cases.exists {
          case CaseDef(Bind(_, Ident(_)), _, _) => true
          case _                                => false
        }
        if !hasDefaultCase then report.errorAndAbort("Match must have a default case", m.pos)

        val (bind, rhs) = cases
          .collectFirst {
            case CaseDef(bind @ Bind(_, typed @ Typed(Ident(_), _)), _, rhs) if tpe <:< typed.symbol.typeRef =>
              (bind, rhs)
          }
          .getOrElse {
            cases.collectFirst { case CaseDef(bind @ Bind(_, Ident(_)), _, rhs) =>
              (bind, rhs)
            }.get
          }

        val res = ValDef.let(owner, bind.name + "Replaced", scrutinee) { newRef =>
          val replacer = new RefReplacer[q.type](bind.symbol, newRef)
          replacer.transformTerm(rhs)(owner)
        }

        res
      case _ => report.errorAndAbort("Body of lateInlineMatch must be a match", aExpr)
    }
  }

  private def processUnrollingDefs[Types <: Tuple: Type, Fields <: Tuple: Type, A: Type](e: Expr[A])(
      using q: Quotes
  ): Expr[A] =
    import q.reflect.*

    val productElementIdExactExpander = new ProductElementIdExactExpander[q.type, Types, Fields]()
    val lateInlineMatchExpander       = new LateInlineMatchExpander[q.type]()

    val r1 = productElementIdExactExpander.transformTerm(e.asTerm)(Symbol.spliceOwner)
    val r2 = lateInlineMatchExpander.transformTerm(r1)(Symbol.spliceOwner)
    r2.asExprOf[A]
  end processUnrollingDefs

  private def tabulateKImpl[A[_]: Type, Types <: Tuple: Type, ElemTop: Type, Fields <: Tuple: Type](
      sizeExpr: Expr[Tuple.Size[Types]],
      f: Expr[(i: Int { type X <: ElemTop }) => A[i.X]],
      unrolling: Expr[Boolean],
      classTagExpr: Expr[ClassTag[A[ElemTop]]]
  )(
      using q: Quotes
  ): Expr[IArray[A[ElemTop]]] = {
    import q.reflect.*

    val doUnrolling = unrolling.valueOrAbort

    val noUnrolling = '{
      given ClassTag[A[ElemTop]] = $classTagExpr

      val arr    = new Array[A[ElemTop]]($sizeExpr)
      var i: Int = 0
      while (i < $sizeExpr) {
        arr(i) = ${ Expr.betaReduce('{ $f(i.asInstanceOf[Int { type X = ElemTop }]) }) }
        i += 1
      }

      arr.asIArray
    }

    val tpes = Helpers.typesOfTuple(TypeRepr.of[Types], Nil)

    val assigns = Seq.tabulate(sizeExpr.valueOrAbort)(i => Expr(i)).zip(tpes.map(_.asType)).map { case (i, '[t]) =>
      (arr: Expr[Array[A[ElemTop]]]) =>
        '{
          $arr($i) = ${
            Expr.betaReduce('{ $f($i.asInstanceOf[Int { type X = t & ElemTop }]).asInstanceOf[A[ElemTop]] })
          }
        }
    }

    val withUnrolling = '{
      given ClassTag[A[ElemTop]] = $classTagExpr
      val arr                    = new Array[A[ElemTop]]($sizeExpr)

      ${ Expr.block(assigns.map(f => f('arr)).toList, '{ arr.asIArray }) }
    }

    if doUnrolling then processUnrollingDefs[Types, Fields, IArray[A[ElemTop]]](withUnrolling) else noUnrolling
  }

  private def tabulateFoldLeftImpl[B: Type, Types <: Tuple: Type, Fields <: Tuple: Type, ElemTop: Type](
      sizeExpr: Expr[Tuple.Size[Types]],
      startExpr: Expr[B],
      unrolling: Expr[Boolean],
      f: Expr[(B, Int { type X <: ElemTop }) => B]
  )(using q: Quotes): Expr[B] = {
    import q.reflect.*

    val doUnrolling = unrolling.valueOrAbort

    val noUnrolling = '{
      var res: B = $startExpr
      var i: Int = 0
      while (i < $sizeExpr) {
        res = ${
          Expr.betaReduce('{ $f(res, i.asInstanceOf[Int { type X <: ElemTop }]) })
        }
        i += 1
      }

      res
    }

    val tpes = Helpers.typesOfTuple(TypeRepr.of[Types], Nil)
    val withUnrolling = Seq.tabulate(sizeExpr.valueOrAbort)(identity).zip(tpes.map(_.asType)).foldLeft(startExpr) {
      case (acc, (i, '[t])) =>
        ConstantType(IntConstant(i)).asType match {
          case '[n] => Expr.betaReduce('{ $f($acc, ${ Expr(i) }.asInstanceOf[Int & n { type X = t & ElemTop }]) })
        }
    }

    if doUnrolling then processUnrollingDefs[Types, Fields, B](withUnrolling) else noUnrolling
  }

  private inline def defaultValue[A]: A =
    val default = inline erasedValue[A] match {
      case _: AnyRef  => null
      case _: Byte    => 0.toByte
      case _: Short   => 0.toShort
      case _: Int     => 0
      case _: Long    => 0L
      case _: Char    => 0.toChar
      case _: Float   => 0F
      case _: Double  => 0D
      case _: Boolean => false
      case _: Unit    => ()
    }
    default.asInstanceOf[A]

  private def traverseKImpl[ElemTop: Type, T <: Tuple: Type, F <: Tuple: Type, A[_]: Type, G[_]: Type, B[_]: Type](
      fa: Expr[IArray[A[ElemTop]]],
      f: Expr[A ~>: Compose2[G, B]],
      G: Expr[Applicative[G]],
      sizeExpr: Expr[Tuple.Size[T]],
      classTagExpr: Expr[ClassTag[B[ElemTop]]]
  )(using q: Quotes): Expr[G[IArray[B[ElemTop]]]] =
    import q.reflect.*

    val isFunctionIdentity = f match {
      case '{ FunctionK.identity } => true
      case _                       => false
    }

    def fIfNotIdentity(obj: Expr[A[ElemTop]]): Expr[G[B[ElemTop]]] =
      if isFunctionIdentity then obj.asInstanceOf[Expr[G[B[ElemTop]]]]
      else '{ $f($obj) }

    def fElem(i: Expr[Int]): Expr[G[B[ElemTop]]] =
      fIfNotIdentity('{ $fa.quickArrayIndex($i) })

    def traverseId = {
      val mapFun = '{ (i: Int { type X <: ElemTop }) =>
        ${ fElem('{ i }) }.asInstanceOf[B[i.X]]
      }
      tabulateKImpl[B, T, ElemTop, F](sizeExpr, mapFun, Expr(false), classTagExpr)
        .asInstanceOf[Expr[G[IArray[B[ElemTop]]]]]
    }

    def traverseEither[E: Type] = '{
      given scala.reflect.ClassTag[B[ElemTop]] = $classTagExpr

      var error: E = defaultValue[E]
      var gotError = false
      val arr      = new Array[B[ElemTop]]($sizeExpr)
      var i: Int   = 0
      while (i < $sizeExpr && !gotError) {
        val res = ${ fElem('i) }.asInstanceOf[Either[E, B[ElemTop]]]
        res match {
          case Right(v) =>
            arr(i) = v
          case Left(e) =>
            gotError = true
            error = e
        }

        i += 1
      }

      val ret: Either[E, IArray[B[ElemTop]]] = if gotError then Left(error) else Right(arr.asIArray)
      ret.asInstanceOf[G[IArray[B[ElemTop]]]]
    }

    def traverseOption = '{
      given scala.reflect.ClassTag[B[ElemTop]] = $classTagExpr
      var gotError                             = false
      val arr                                  = new Array[B[ElemTop]]($sizeExpr)
      var i: Int                               = 0
      while (i < $sizeExpr && !gotError) {
        val res = ${ fElem('i) }.asInstanceOf[Option[B[ElemTop]]]
        if (res.isEmpty) {
          gotError = true
        } else {
          arr(i) = res.get
        }

        i += 1
      }

      val ret: Option[IArray[B[ElemTop]]] = if gotError then None else Some(arr.asIArray)
      ret.asInstanceOf[G[IArray[B[ElemTop]]]]
    }

    G match {
      case '{ cats.Invariant.catsInstancesForId }                  => traverseId
      case '{ cats.catsInstancesForId }                            => traverseId
      case '{ cats.instances.either.catsStdInstancesForEither[e] } => traverseEither[e]
      case '{ cats.Invariant.catsMonadErrorForEither[e] }          => traverseEither[e]
      case '{ cats.instances.option.catsStdInstancesForOption }    => traverseOption
      case '{ cats.Invariant.catsInstancesForOption }              => traverseOption
      case _ =>
        '{
          val it = $fa.iterator

          var acc: G[List[B[ElemTop]]] = $G.pure(List.empty[B[ElemTop]])
          while (it.hasNext) {
            val obj = it.next()
            acc = $G.map2(${ fIfNotIdentity('obj) }, acc)((v, a) => v :: a)
          }

          $G.map(acc)(a => iteratorToArray(a.reverseIterator, $sizeExpr)(using $classTagExpr).asIArray)
        }
    }
  end traverseKImpl

  private def tabulateTraverseKImpl[ElemTop: Type, Types <: Tuple: Type, Fields <: Tuple: Type, G[_]: Type, B[_]: Type](
      f: Expr[(i: Int { type X <: ElemTop }) => G[B[i.X]]],
      unrolling: Expr[Boolean],
      G: Expr[Applicative[G]],
      sizeExpr: Expr[Tuple.Size[Types]],
      classTagExpr: Expr[ClassTag[B[ElemTop]]]
  )(using q: Quotes): Expr[G[IArray[B[ElemTop]]]] =
    import q.reflect.*

    val doUnrolling = unrolling.valueOrAbort
    val size        = sizeExpr.valueOrAbort

    def traverseId =
      tabulateKImpl[B, Types, ElemTop, Fields](
        sizeExpr,
        f.asInstanceOf[Expr[(i: Int { type X <: ElemTop }) => B[i.X]]],
        unrolling,
        classTagExpr
      ).asInstanceOf[Expr[G[IArray[B[ElemTop]]]]]

    def fApply(i: Expr[Int]): Expr[G[B[ElemTop]]] =
      Expr.betaReduce('{ $f($i.asInstanceOf[Int { type X = ElemTop }]) })

    def traverseEither[E: Type] = {
      val withoutUnrolling = '{
        given scala.reflect.ClassTag[B[ElemTop]] = $classTagExpr

        var error: E = defaultValue[E]
        var gotError = false
        val arr      = new Array[B[ElemTop]]($sizeExpr)
        var i: Int   = 0
        while (i < $sizeExpr && !gotError) {
          val res = ${ fApply('i) }.asInstanceOf[Either[E, B[ElemTop]]]
          res match {
            case Right(v) =>
              arr(i) = v
            case Left(e) =>
              gotError = true
              error = e
          }

          i += 1
        }

        val ret: Either[E, IArray[B[ElemTop]]] = if gotError then Left(error) else Right(arr.asIArray)
        ret.asInstanceOf[G[IArray[B[ElemTop]]]]
      }

      val withUnrolling = '{
        val arr = new Array[B[ElemTop]]($sizeExpr)

        val ret = ${
          List.tabulate(size)(i => Expr(i)).foldRight('{ Right(arr.asIArray): Either[E, IArray[B[ElemTop]]] }) { (i, acc) =>
            '{
              ${ fApply(i) }.asInstanceOf[Either[E, B[ElemTop]]] match {
                case Right(v) =>
                  arr($i) = v
                  $acc
                case Left(e) =>
                  Left(e)
              }
            }
          }
        }

        ret.asInstanceOf[G[IArray[B[ElemTop]]]]
      }

      if doUnrolling then processUnrollingDefs[Types, Fields, G[IArray[B[ElemTop]]]](withUnrolling)
      else withoutUnrolling
    }

    def traverseOption = {
      val withoutUnrolling = '{
        given scala.reflect.ClassTag[B[ElemTop]] = $classTagExpr

        var gotError = false
        val arr      = new Array[B[ElemTop]]($sizeExpr)
        var i: Int   = 0
        while (i < $sizeExpr && !gotError) {
          val res = ${ fApply('i) }.asInstanceOf[Option[B[ElemTop]]]
          if (res.isEmpty) {
            gotError = true
          } else {
            arr(i) = res.get
          }

          i += 1
        }

        val ret: Option[IArray[B[ElemTop]]] = if gotError then None else Some(arr.asIArray)
        ret.asInstanceOf[G[IArray[B[ElemTop]]]]
      }

      val withUnrolling = '{
        val arr = new Array[B[ElemTop]]($sizeExpr)

        val ret = ${
          List.tabulate(size)(i => Expr(i)).foldRight('{ Some(arr.asIArray): Option[IArray[B[ElemTop]]] }) { (i, acc) =>
            '{
              ${ fApply(i) }.asInstanceOf[Option[B[ElemTop]]] match {
                case Some(v) =>
                  arr($i) = v
                  $acc
                case None => None
              }
            }
          }
        }

        ret.asInstanceOf[G[IArray[B[ElemTop]]]]
      }

      if doUnrolling then processUnrollingDefs[Types, Fields, G[IArray[B[ElemTop]]]](withUnrolling)
      else withoutUnrolling
    }

    G match {
      case '{ cats.Invariant.catsInstancesForId }                  => traverseId
      case '{ cats.catsInstancesForId }                            => traverseId
      case '{ cats.instances.either.catsStdInstancesForEither[e] } => traverseEither[e]
      case '{ cats.Invariant.catsMonadErrorForEither[e] }          => traverseEither[e]
      case '{ cats.instances.option.catsStdInstancesForOption }    => traverseOption
      case '{ cats.Invariant.catsInstancesForOption }              => traverseOption
      case _ =>
        val withoutUnrolling = '{
          var acc: G[List[B[ElemTop]]] = $G.pure(List.empty[B[ElemTop]])
          var i: Int                   = 0
          while (i < $sizeExpr) {
            acc = $G.map2(${ fApply('i) }, acc)((v, a) => v :: a)
            i += 1
          }

          $G.map(acc)(a => iteratorToArray(a.reverseIterator, $sizeExpr)(using $classTagExpr).asIArray)
        }

        val withUnrolling = {
          val mapped = List.tabulate(size)(i => Expr(i)).foldLeft('{ $G.pure(List.empty[B[ElemTop]]) }) { (acc, i) =>
            '{ $G.map2(${ fApply(i) }, $acc)((v, a) => v :: a) }
          }

          '{ $G.map($mapped)(a => iteratorToArray(a.reverseIterator, $sizeExpr)(using $classTagExpr).asIArray) }
        }

        if doUnrolling then processUnrollingDefs[Types, Fields, G[IArray[B[ElemTop]]]](withUnrolling)
        else withoutUnrolling
    }
  end tabulateTraverseKImpl

  sealed trait InlineHKDGenericTypeclassOps[A, T <: Tuple, F <: Tuple] extends InlineHKDGeneric[A]:
    override type Gen[F[_]] = IArray[F[ElemTop]]
    override type Index     = Int { type X <: ElemTop }

    override inline def nameToIndex[Name <: this.Names](name: Name): IndexAux[this.FieldOf[Name]] =
      // TODO: Macro match
      val n = names
      val res = tabulateFoldLeft(Nil: List[(this.Names, Index)]) { (acc, idx) =>
        val name = n.indexK(idx)
        (name, idx) :: acc
      }

      res.find(_._1 == name).get._2.asInstanceOf[IndexAux[this.FieldOf[Name]]]

    inline def size: Tuple.Size[T]

    extension [A[_]](fa: Gen[A])
      override inline def foldLeftK[B](inline b: B)(inline f: B => A ~>#: B): B =
        var i: Int = 0
        var res: B = b
        while (i < size) {
          res = f(res)(fa(i))
          i += 1
        }

        res
      end foldLeftK

      override inline def foldMapK[B](inline f: A ~>#: B)(using B: Monoid[B]): B =
        foldLeftK(B.empty)(b => [Z] => (az: A[Z]) => b.combine(f(az)))

    extension [A](fa: Gen[Const[A]])
      override inline def toListK: List[A] =
        fa.toList

    extension [A[_]](fa: Gen[A])

      // Traverse

      override inline def traverseK[G[_], B[_]](inline f: A ~>: Compose2[G, B])(
          using inline G: Applicative[G],
          classTag: ClassTag[B[ElemTop]]
      ): G[Gen[B]] =
        ${ traverseKImpl[ElemTop, T, F, A, G, B]('fa, 'f, 'G, 'size, 'classTag) }
      end traverseK

    extension [G[_], A[_]](fga: Gen[Compose2[G, A]])
      override inline def sequenceK(using inline G: Applicative[G]): G[Gen[A]] =
        fga.traverseK[G, A](FunctionK.identity)

    extension [A[_]](fa: Gen[A])
      // Representable

      override inline def indexK(rep: Index): A[rep.X] =
        fa.quickArrayIndex(rep).asInstanceOf[A[rep.X]]

    override inline def tabulateK[A[_]](inline f: (i: Index) => A[i.X], inline unrolling: Boolean)(
        using classTag: ClassTag[A[ElemTop]]
    ): Gen[A] =
      ${ tabulateKImpl[A, T, ElemTop, F]('size, 'f, 'unrolling, 'classTag) }
    end tabulateK

    override inline def tabulateFoldLeft[B](inline start: B, inline unrolling: Boolean)(inline f: (B, Index) => B): B =
      ${ tabulateFoldLeftImpl[B, T, F, ElemTop]('size, 'start, 'unrolling, 'f) }
    end tabulateFoldLeft

    override inline def tabulateTraverseK[G[_], B[_]](inline f: (i: Index) => G[B[i.X]], inline unrolling: Boolean)(
        using inline G: Applicative[G],
        classTag: ClassTag[B[ElemTop]]
    ): G[Gen[B]] =
      ${ tabulateTraverseKImpl[ElemTop, T, F, G, B]('f, 'unrolling, 'G, 'size, 'classTag) }

    // Monad

    extension [A[_]](fa: Gen[A])
      override inline def flatMapK[B[_]](inline f: A ~>: Const[Gen[B]]): Gen[B] =
        tabulateK(r => f(fa.indexK(r)).indexK(r))

    // Distributive

    extension [G[_]: Functor, A[_]](gfa: G[Gen[A]])
      override inline def cosequenceK: Gen[Compose2[G, A]] =
        tabulateK(r => gfa.map(fa => fa.indexK(r)))

    // Applicative

    extension [A[_]](a: ValueK[A])
      override inline def pure: Gen[A] =
        tabulateK(r => a[r.X]())

    // Apply

    extension [A[_]](fa: Gen[A])
      override inline def map2K[B[_], Z[_]](fb: Gen[B])(inline f: [X] => (A[X], B[X]) => Z[X]): Gen[Z] =
        tabulateK(r => f(fa.indexK(r), fb.indexK(r)))

    // Functor
    extension [A[_]](fa: Gen[A])
      override inline def mapK[B[_]](inline f: A ~>: B): Gen[B] =
        tabulateK(r => f(fa.indexK(r)))

  end InlineHKDGenericTypeclassOps

  private[perspective] type Names[ElemLabels <: Tuple] = TupleUnionLub[ElemLabels, String, Nothing]

  private[perspective] inline def stringToName[ElemLabels <: Tuple](
      s: String
  ): Option[Names[ElemLabels]] =
    val namesSet = Helpers.constValueTupleToSet[ElemLabels].asInstanceOf[Set[String]]
    Option.when(namesSet(s))(s.asInstanceOf[Names[ElemLabels]])

end InlineHKDGeneric

/**
  * A type like [[HKDProductGeneric]] but where as much as possible is defined
  * inline, and tries to generate as simple bytecode as possible. Might be more
  * error prone, and produce bigger classes using this, but the bytecode is much
  * simpler.
  * @tparam A
  *   The type being abstracted over.
  */
trait InlineHKDProductGeneric[A] extends InlineHKDGeneric[A]:
  /** Convert a value of [[A]] to the higher kinded representation. */
  inline def to(a: A): Gen[Id]

  /** Convert a value of the higher kinded representation to [[A]]. */
  inline def from(gen: Gen[Id]): A

  extension (a: A)

    /** Equivalent to `to(a).indexK(index)`, but faster. */
    inline def productElementId(index: Index): index.X

    /**
      * Like [[productElementId]], but gives back the exact type. Can only be
      * used in unrolled calls.
      */
    transparent inline def productElementIdExact(inline idx: Index): idx.X

  /**
    * Like an inline match, but delays the expansion slightly. The value inside
    * must be a normal match. Can only be used in unrolled calls.
    */
  inline def lateInlineMatch[A](inline a: A): A = InlineHKDGeneric.lateInlineMatch(a)

object InlineHKDProductGeneric:
  transparent inline def apply[A](using gen: InlineHKDProductGeneric[A]): InlineHKDProductGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = InlineHKDProductGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  transparent inline given derived[A](
      using m: Mirror.ProductOf[A],
      idClassTag: ClassTag[Tuple.Union[m.MirroredElemTypes]]
  ): DerivedImpl[A, m.MirroredElemTypes, m.MirroredElemLabels, m.MirroredLabel, HKDGeneric.TupleUnionLub[
    m.MirroredElemLabels,
    String,
    Nothing
  ]] =
    // Yes, we really do have to compute the names union here. Otherwise it seems Scala partially forgets what it is
    type Names = HKDGeneric.TupleUnionLub[m.MirroredElemLabels, String, Nothing]
    new DerivedImpl[A, m.MirroredElemTypes, m.MirroredElemLabels, m.MirroredLabel, Names](using m)

  class DerivedImpl[A, ElemTypes <: Tuple, ElemLabels <: Tuple, TypeName0 <: String, NamesUnion <: String](
      using val m: Mirror.ProductOf[A] {
        type MirroredElemTypes = ElemTypes; type MirroredElemLabels = ElemLabels; type MirroredLabel = TypeName0
      },
      val idClassTag: ClassTag[Tuple.Union[ElemTypes]]
  ) extends InlineHKDProductGeneric[A]
      with InlineHKDGeneric.InlineHKDGenericTypeclassOps[A, ElemTypes, ElemLabels]:
    override type Gen[F[_]] = IArray[F[ElemTop]]
    // override opaque type Index = Int { type X <: ElemTop }
    override type ElemTop = Tuple.Union[ElemTypes]

    override type TypeName = m.MirroredLabel
    override inline def typeName: TypeName = constValue[m.MirroredLabel]

    override inline def size: Tuple.Size[ElemTypes] = constValue[Tuple.Size[ElemTypes]]

    override type Names = NamesUnion
    override inline def names: Gen[Const[Names]] =
      Helpers.constValueTupleToIArray[ElemLabels].asInstanceOf[Gen[Const[Names]]]

    override inline def stringToName(s: String): Option[Names] =
      InlineHKDGeneric.stringToName[ElemLabels](s).asInstanceOf[Option[Names]] // In theory harmless cast

    override type FieldOf[Name <: Names] = InlineHKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, ElemLabels]

    override type TupleRep = ElemTypes

    override inline def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F] =
      Tuple.fromIArray(gen).asInstanceOf[Tuple.Map[TupleRep, F]]

    override inline def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F] =
      InlineHKDGeneric.asIArray(
        InlineHKDGeneric.iteratorToArray(tuple.productIterator.asInstanceOf[Iterator[F[ElemTop]]], size)
      )

    override inline def to(a: A): Gen[Id] =
      InlineHKDGeneric.asIArray(
        InlineHKDGeneric.iteratorToArray(
          a.asInstanceOf[Product].productIterator.asInstanceOf[Iterator[Id[ElemTop]]],
          size
        )(using idClassTag)
      )

    override inline def from(a: Gen[Id]): A =
      inline a match
        case arr: IArray[Object] => m.fromProduct(ArrayProduct(arr))
        case _                   => m.fromProduct(ArrayProduct(a.map(_.asInstanceOf[Object])))

    extension (a: A)
      override inline def productElementId(index: Index): index.X =
        a.asInstanceOf[Product].productElement(index).asInstanceOf[index.X]

      override transparent inline def productElementIdExact(inline idx: Index): idx.X =
        InlineHKDGeneric.productElementIdExact[A, ElemTop](a, idx)

    override inline given summonInstances[F[_]]: Gen[F] =
      Helpers.summonAllToIArray[Tuple.Map[ElemTypes, F]].asInstanceOf[Gen[F]]
  end DerivedImpl

/**
  * A type like [[HKDSumGeneric]] but where as much as possible is defined
  * inline, and tries to generate as simple bytecode as possible. Might be more
  * error prone, and produce bigger classes using this, but the bytecode is much
  * simpler.
  * @tparam A
  *   The type being abstracted over.
  */
trait InlineHKDSumGeneric[A] extends InlineHKDGeneric[A]:

  override type ElemTop <: A

  /** Returns the name of a field given the type of the field. */
  type NameOf[X <: ElemTop] <: Names

  /**
    * Returns the index of a value. Because of soundness, this method can not be
    * used if X = A. In that case, use [[indexOfA]] instead.
    */
  inline def indexOf[X <: ElemTop](x: X): IndexAux[X]

  /** Same as [[indexOf]] but also works for values of type A. */
  inline def indexOfA(a: A): Index = indexOf(a.asInstanceOf[ElemTop])

  /**
    * Same as [[indexOfA]] but also essentially cats the value to the unknown
    * type, allowing further operations on it that requires that it is a subtype
    * of A.
    */
  inline def indexOfACasting(a: A): InlineHKDSumGeneric.IndexOfACasting[Index, ElemTop]

  /** Given a index, return the name of the index. */
  inline def indexToName(idx: Index): NameOf[idx.X]

  /**
    * Widen the higher kinded representation to a [[Const]] type of the top
    * type.
    */
  extension [F[+_]](gen: Gen[F])
    inline def widenConst: Gen[Const[F[A]]] =
      // This is safe. We can't use the widen method as it can't know about the contents of Gen, we do
      gen.asInstanceOf[Gen[Const[F[A]]]]

  /**
    * Convert a value of [[A]] to the higher kinded representation. It will be
    * Some in only one field, corresponding to the subtype passed in, and None
    * in all the others.
    */
  inline def to(a: A): Gen[Option] =
    val index = indexOfA(a)
    // This cast is safe as we know A = Z
    tabulateK(i => if i == index then Some(a.asInstanceOf[i.X]) else None)

  /**
    * Convert a value of the higher kinded representation to [[A]]. Will only
    * return Some if only one of the fields is Some and the rest is None.
    */
  inline def from(a: Gen[Option]): Option[A]

//noinspection ScalaUnusedSymbol,DuplicatedCode
object InlineHKDSumGeneric:

  trait IndexOfACasting[Index <: Any { type X <: ElemTop }, ElemTop] {
    type X0 <: ElemTop
    val index: Index { type X = X0 }
    val value: X0
  }
  object IndexOfACasting {
    class IndexOfACastingImpl[Index <: Any { type X <: ElemTop }, ElemTop, X1 <: ElemTop](
        val index: Index { type X = X1 },
        val value: X1
    ) extends IndexOfACasting[Index, ElemTop] {
      type X0 = X1
    }
  }

  def apply[A](using gen: InlineHKDSumGeneric[A]): InlineHKDSumGeneric.Aux[A, gen.Gen] = gen

  type Aux[A, Gen0[_[_]]] = InlineHKDSumGeneric[A] {
    type Gen[B[_]] = Gen0[B]
  }

  type NameOfImpl[Names, X, ElemTypes, Labels] <: Names = (ElemTypes, ElemTypes, Labels) match {
    case (X *: tt, th *: _, lh *: lt) =>
      Helpers.Eq[th, X] match {
        case true => lh & Names
      }
    case (th *: tt, _, lh *: lt) => NameOfImpl[Names, X, tt, lt]
  }

  transparent inline given derived[A](
      using m: Mirror.SumOf[A],
      idClassTag: ClassTag[InlineHKDGeneric.TupleUnionLub[m.MirroredElemTypes, A, Nothing]]
  ): DerivedImpl[A, m.MirroredElemTypes, m.MirroredElemLabels, m.MirroredLabel] =
    new DerivedImpl[A, m.MirroredElemTypes, m.MirroredElemLabels, m.MirroredLabel](using m, idClassTag)

  class DerivedImpl[A, ElemTypes <: Tuple, ElemLabels <: Tuple, TypeName0 <: String](
      using val m: Mirror.SumOf[A] {
        type MirroredElemTypes  = ElemTypes
        type MirroredElemLabels = ElemLabels
        type MirroredLabel      = TypeName0
      },
      val idClassTag: ClassTag[InlineHKDGeneric.TupleUnionLub[ElemTypes, A, Nothing]]
  ) extends InlineHKDSumGeneric[A]
      with InlineHKDGeneric.InlineHKDGenericTypeclassOps[A, ElemTypes, ElemLabels]:
    override type Gen[F[_]] = IArray[F[ElemTop]]
    override type ElemTop   = InlineHKDGeneric.TupleUnionLub[ElemTypes, A, Nothing]

    override type TypeName = m.MirroredLabel

    override inline def typeName: TypeName = constValue[m.MirroredLabel]

    override inline def size: Tuple.Size[ElemTypes] = constValue[Tuple.Size[ElemTypes]]

    override type Names = InlineHKDGeneric.Names[ElemLabels]

    override inline def names: Gen[Const[Names]] =
      Helpers.constValueTupleToIArray[ElemLabels].asInstanceOf[Gen[Const[Names]]]

    override inline def stringToName(s: String): Option[Names] =
      InlineHKDGeneric.stringToName[ElemLabels](s)

    override type FieldOf[Name <: Names] = InlineHKDGeneric.FieldOfImpl[Name, ElemTop, ElemTypes, ElemLabels]

    override type TupleRep = ElemTypes

    override inline def genToTuple[F[_]](gen: Gen[F]): Tuple.Map[TupleRep, F] =
      Tuple.fromIArray(gen).asInstanceOf[Tuple.Map[TupleRep, F]]

    override inline def tupleToGen[F[_]](tuple: Tuple.Map[TupleRep, F]): Gen[F] =
      InlineHKDGeneric.asIArray(
        InlineHKDGeneric.iteratorToArray(tuple.productIterator.asInstanceOf[Iterator[F[ElemTop]]], size)
      )

    inline def from(a: Gen[Option]): Option[A] =
      val res = a.widenConst.flatten
      if res.length == 1 then Some(res.head)
      else None
    end from

    inline def indexOf[X <: ElemTop](x: X): IndexAux[X] =
      m.ordinal(x).asInstanceOf[IndexAux[X]]

    inline def indexOfACasting(a: A): InlineHKDSumGeneric.IndexOfACasting[Index, ElemTop] =
      val idx = indexOfA(a)
      new InlineHKDSumGeneric.IndexOfACasting.IndexOfACastingImpl[Index, ElemTop, idx.X](idx, a.asInstanceOf[idx.X])

    inline def indexToName(idx: Index): NameOf[idx.X] =
      names(idx).asInstanceOf[NameOf[idx.X]]

    override inline given summonInstances[F[_]]: Gen[F] =
      Helpers.summonAllToIArray[Tuple.Map[ElemTypes, F]].asInstanceOf[Gen[F]]
  end DerivedImpl
