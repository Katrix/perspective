package perspective.macros

import cats.tagless.{DeriveMacros => CatsDeriveMacros}
import perspective._

import scala.annotation.tailrec
import scala.reflect.macros.whitebox

private[macros] class DeriveMacros(override val c: whitebox.Context) extends CatsDeriveMacros(c) {
  import c.internal._
  import c.universe._

  case class Param(
      name: TermName,
      tpe: Type,
      body: Tree
  ) {

    def transform(instance: Symbol, types: (Symbol, Symbol)*)(body: PartialFunction[Tree, Tree]): Param = {
      val (from, to) = types.toList.unzip
      copy(
        tpe = tpe.substituteSymbols(from, to),
        body = body.applyOrElse(q"$instance.$name", identity[Tree])
      )
    }

    def accessor(to: Tree): Tree = q"$to.$name"
  }

  def newClass(algebra: Type)(typeArgs: Type*)(parameters: Iterable[Iterable[Tree]]): Tree = {
    val applied = appliedType(algebra, typeArgs.toList)

    q"""new $applied(...$parameters)"""
  }

  def allParams(algebra: Type): List[List[Symbol]] = {
    val paramLists = algebra.typeSymbol.asClass.primaryConstructor.asMethod.paramLists

    for {
      params <- paramLists
      param  <- params
    } {
      val field = algebra.decl(param.name)

      if (field == NoSymbol) {
        c.abort(c.enclosingPosition, s"No field for ${param.fullName} found")
      }

      if (!field.asMethod.isAccessor) {
        c.abort(c.enclosingPosition, s"No accessor for ${param.fullName} found")
      }
    }

    paramLists
  }

  def hkdParamSize(algebra: Type): List[List[Int]] = createParameters(q"$NoSymbol", algebra).map(_.map {
    case p if isMaybeHKD(p.tpe) => hkdSize(p.tpe)
    case _                      => 1
  })

  def hkdSize(algebra: Type): Int =
    hkdParamSize(algebra).flatten.sum

  def createParameters(from: Tree, algebra: Type): List[List[Param]] = {
    allParams(algebra).map(_.map { param =>
      val name  = param.name.toTermName
      val field = algebra.decl(param.name)
      val body  = q"$from.$name"

      Param(name, field.typeSignatureIn(algebra).finalResultType, body)
    })
  }

  def transformParameters(instance: Tree, algebra: Type)(
      transform: PartialFunction[Param, Param]
  ): List[List[Tree]] =
    createParameters(instance, algebra).map(_.map(p => transform.applyOrElse(p, identity[Param]).body))

  def typeParamsForAlgebra(algebra: Type)(types: Type*): Seq[Type] =
    types.take(algebra.typeParams.length)

  def typeParamsForAlgebraFromSymbols(algebra: Type)(types: Symbol*): Seq[Type] =
    types.map(_.asType.toTypeConstructor).take(algebra.typeParams.length)

  //def mapK[A[_], B[_], C](fa: F[A, C])(f: A ~>: B): F[B, C]
  def perspectiveMapK(algebra: Type): (String, Type => Tree) = "mapK" -> {
    case PolyType(List(a, b, c), MethodType(List(fa), MethodType(List(f), _))) =>
      val Fa = singleType(NoPrefix, fa)

      val parameters = transformParameters(q"$fa", Fa) {
        case param if occursIn(param.tpe)(a) =>
          param.transform(fa, a -> b) {
            case accessor =>
              val F = summon[FunctorK[Any]](polyType(a :: c :: Nil, param.tpe))
              q"$F.mapK[$a, $b, $c]($accessor)($f)"
          }
      }

      newClass(algebra)(typeParamsForAlgebraFromSymbols(algebra)(b, c): _*)(parameters)
  }

  //def map2K[A[_], B[_], Z[_], C](fa: F[A, C], fb: F[B, C])(f: Tuple2K[A, B]#λ ~>: Z): F[Z, C]
  def map2K(algebra: Type): (String, Type => Tree) = {
    val Tuple2K = symbolOf[Tuple2K[Any, Any, Any]]
    "map2K" -> {
      case PolyType(List(a, b, z, c), MethodType(List(fa, fb), MethodType(List(f), _))) =>
        val A   = a.asType.toTypeConstructor
        val B   = b.asType.toTypeConstructor
        val t2k = polyType(A.typeParams, appliedType(Tuple2K, A :: B :: A.typeParams.map(_.asType.toType)))
        val Fa  = singleType(NoPrefix, fa)

        val parameters = transformParameters(q"$fa", Fa) {
          case param if occursIn(param.tpe)(a) =>
            val tpe = param.tpe.map(t => if (t.typeSymbol == a) appliedType(t2k, t.typeArgs) else t)
            val F   = summon[ApplyK[Any]](polyType(a :: c :: Nil, param.tpe))

            param.copy(
              tpe = tpe,
              body = q"$F.map2K[$a, $b, $z, $c](${param.accessor(q"$fa")}, ${param.accessor(q"$fb")})($f)"
            )
        }
        newClass(algebra)(typeParamsForAlgebraFromSymbols(algebra)(z, c): _*)(parameters)
    }
  }

  //def pureK[A[_], C](a: Unit #~>: A): F[A, C]
  def pureK(algebra: Type): (String, Type => Tree) = "pureK" -> {
    case PolyType(List(at, c), MethodType(List(av), fa)) =>
      val Fa = fa

      //Can't do occursIn because we don't have a value to insert if the type isn't A[B] for some B
      val parameters = createParameters(q"$NoSymbol", Fa).map(_.map { param =>
        val F = summon[ApplicativeK[Any]](polyType(at :: c :: Nil, param.tpe))
        q"$F.pureK[$at, $c]($av)"
      })

      newClass(algebra)(typeParamsForAlgebraFromSymbols(algebra)(at, c): _*)(parameters)
  }

  //def foldLeftK[A[_], B, C](fa: F[A, C], b: B)(f: B => A ~>#: B): B
  def foldLeftK(algebra: Type): (String, Type => Tree) = "foldLeftK" -> {
    case PolyType(List(a, bt, c), MethodType(List(fa, bv), MethodType(List(f), _))) =>
      val Fa = singleType(NoPrefix, fa)

      createParameters(q"$fa", Fa).flatten.toVector.filter(p => occursIn(p.tpe)(a)).reverse.foldLeft(q"$bv") {
        (bn, param) =>
          val F = summon[FoldableK[Any]](polyType(a :: c :: Nil, param.tpe))
          q"$F.foldLeftK[$a, $bt, $c](${param.body}, $bn)($f)"
      }
  }

  //def traverseK[G[_]: Applicative, A[_], B[_], C](fa: F[A, C])(f: A ~>: Compose[G, B, *]): G[F[B, C]]
  def traverseKFunc(algebra: Type): (String, Type => Tree) = "traverseK" -> {
    case PolyType(List(g, a, b, c), MethodType(List(fa), MethodType(List(f), MethodType(List(gApp), _)))) =>
      val Fa = singleType(NoPrefix, fa)
      val G  = gApp

      //TODO: occursIn?

      val paramLists = createParameters(q"$fa", Fa).toVector

      if (paramLists.sizeIs > 1) {
        val (sizes, tuples) = paramLists.map { paramList =>
          val size      = paramList.size
          val tupleName = TermName(s"tuple$size")
          val args = paramList.map { p =>
            val F = summon[TraverseK[Any]](polyType(a :: c :: Nil, p.tpe))
            q"$F.traverseK[$g, $a, $b, $c](${p.body})($f)"
          }

          size -> q"$G.$tupleName(..$args)"
        }.unzip

        val size    = paramLists.size
        val mapName = if (size == 1) TermName(s"map") else TermName(s"map$size")

        val mapBody = {
          val funcParams = (0 until size).map { i =>
            val tupleSize = sizes(i)
            val params    = paramLists(i)

            val tupleNameSimple = TypeName(s"Tuple$tupleSize")
            val tupleName       = tq"_root_.scala.$tupleNameSimple"

            val subsitutedTupleTypes = params.map(p => tq"${p.tpe.substituteSymbols(List(a), List(b))}")

            val tupleParamType = tq"$tupleName[..$subsitutedTupleTypes]"

            ValDef(Modifiers(Flag.PARAM), TermName(this.c.freshName(i.toString)), tupleParamType, EmptyTree)
          }

          val params = funcParams.zipWithIndex.map {
            case (param, idx) =>
              (1 to sizes(idx)).map { i =>
                val tupleAccessor = TermName(s"_$i")
                q"${param.name}.$tupleAccessor"
              }
          }

          q"(..$funcParams) => ${newClass(algebra)(typeParamsForAlgebraFromSymbols(algebra)(b, c): _*)(params)}"
        }

        q"$G.$mapName(..$tuples)($mapBody)"
      } else {
        val paramList = paramLists.head
        val size      = paramList.size

        val args = paramList.map { p =>
          val F = summon[TraverseK[Any]](polyType(a :: c :: Nil, p.tpe))
          q"$F.traverseK[$g, $a, $b, $c](${p.body})($f)"
        }

        val mapName = if (size == 1) TermName(s"map") else TermName(s"map$size")

        val mapBody = {
          val funcParams = paramList.map { p =>
            val tpe = tq"${p.tpe.substituteSymbols(List(a), List(b))}"
            ValDef(Modifiers(Flag.PARAM), TermName(this.c.freshName(p.name.toString)), tpe, EmptyTree)
          }

          val params = funcParams.map(param => q"${param.name}")

          q"(..$funcParams) => ${newClass(algebra)(typeParamsForAlgebraFromSymbols(algebra)(b, c): _*)(List(params))}"
        }

        q"$G.$mapName(..$args)($mapBody)"
      }
  }

  //def cosequenceK[G[_]: Functor, A[_], C](gfa: G[F[A, C]]): F[Compose[G, A, *], C]
  def cosequenceK(algebra: Type): (String, Type => Tree) = "cosequenceK" -> {
    case PolyType(List(g, a, c), MethodType(List(gfa), MethodType(List(gImp), _))) =>
      val Compose = symbolOf[Compose2[Any, Any, Any]]
      val G       = g.asType.toTypeConstructor
      val A       = a.asType.toTypeConstructor

      val Fa = gfa.typeSignatureIn(algebra).typeArgs.head

      val lambdaName  = TermName(this.c.freshName())
      val lambdaParam = ValDef(Modifiers(Flag.PARAM), lambdaName, tq"$Fa", EmptyTree)

      val parameters = transformParameters(q"$lambdaName", Fa) {
        case param if occursIn(param.tpe)(a) =>
          val F = summon[DistributiveK[Any]](polyType(a :: c :: Nil, param.tpe))

          param.copy(body = q"$F.cosequenceK[$g, $a, $c]($gImp.map($gfa)($lambdaParam => ${param.body}))")
        case paramNotOccurs =>
          paramNotOccurs.copy(body = q"$gfa.map($lambdaParam => ${paramNotOccurs.body})")
      }

      val composeGA = polyType(G.typeParams, appliedType(Compose, G :: A :: G.typeParams.map(_.asType.toType)))

      newClass(algebra)(
        typeParamsForAlgebra(algebra)(composeGA.typeConstructor, c.asType.toTypeConstructor): _*
      )(parameters)
  }

  def newFunctionK(tpe: Type, body: Method => Tree) = {
    val members = overridableMembersOf(tpe).filter(_.isAbstract)
    val stubs = delegateMethods(tpe, members, NoSymbol) {
      case m => m.copy(body = body(m))
    }
    val apply = stubs.head

    q"""new $tpe { $apply }"""
  }

  //def indexK[A[_], C](fa: F[A, C]): RepresentationK ~>: A
  def indexK(algebra: Type): (String, Type => Tree) = "indexK" -> {
    case PolyType(List(at, c), MethodType(List(fa), functionK @ TypeRef(_, _, List(from, to)))) =>
      val Fa = singleType(NoPrefix, fa)

      def cases(returnType: Type) =
        createParameters(q"$fa", Fa).flatten
          .zip(hkdParamSize(algebra).flatten)
          .foldLeft((0, List.empty[Tree])) {
            case ((n, acc), (param, size)) =>
              if (size == 1) {
                (n + 1, cq"$n => ${param.body}.asInstanceOf[$returnType]" :: acc)
              } else {
                val fullType =
                  tq"_root_.perspective.RepresentableK[${polyType(at :: c :: Nil, param.tpe)}] { type RepresentationK[_] = _root_.perspective.Finite[$size]}"
                val F = this.c
                  .inferImplicitValue(this.c.typecheck(fullType, mode = this.c.TYPEmode).tpe)
                  .orElse(abort(s"could not find implicit value of type $fullType"))

                (
                  n + size,
                  cq"i if i >= $n && i < $n + $size => $F.indexK(${param.body})(_root_.perspective.Finite($size, i - $n))" :: acc
                )
              }
          }
          ._2
          .reverse

      newFunctionK(functionK, m => q"fa.value match { case ..${cases(m.returnType)} }")
  }

  //def tabulateK[A[_], C](f: RepresentationK ~>: A): F[A, C]
  def tabulateK(algebra: Type): (String, Type => Tree) = "tabulateK" -> {
    case PolyType(List(at, c), MethodType(List(functionK), fa)) =>
      val Fa = fa

      val n = hkdSize(algebra)

      //Can't do occursIn because we don't have a value to insert if the type isn't A[B] for some B
      val parameters = createParameters(q"$NoSymbol", Fa)
        .zip(hkdParamSize(algebra))
        .foldLeft((0, List.empty[List[Tree]])) {
          case ((topI, topAcc), (params, sizes)) =>
            val (newI, paramBlock) = params.zip(sizes).foldLeft((topI, List.empty[Tree])) {
              case ((i, acc), (param, size)) =>
                if (size == 1) {
                  (i + 1, q"$functionK(_root_.perspective.Finite($n, $i))" :: acc)
                } else {
                  val finiteSize = tq"_root_.perspective.Finite[$size]"
                  val fullType =
                    tq"_root_.perspective.RepresentableK[${polyType(at :: c :: Nil, param.tpe)}] { type RepresentationK[_] = $finiteSize}"
                  val F = this.c
                    .inferImplicitValue(this.c.typecheck(fullType, mode = this.c.TYPEmode).tpe)
                    .orElse(abort(s"could not find implicit value of type $fullType"))

                  val newFunctionKTpe = this.c
                    .typecheck(
                      tq"_root_.perspective.FunctionK[({type L[Z] = $finiteSize})#L, $at]",
                      mode = this.c.TYPEmode
                    )
                    .tpe
                  
                  val createdFunctionK = newFunctionK(
                    newFunctionKTpe,
                    m => q"$functionK(_root_.perspective.Finite($n, $i + ${m.paramLists.flatten.head.name}.value))"
                  )

                  (
                    i + size, q"$F.tabulateK($createdFunctionK)" :: acc
                  )
                }
            }

            (newI, paramBlock.reverse :: topAcc)
        }
        ._2
        .reverse

      newClass(algebra)(typeParamsForAlgebraFromSymbols(algebra)(at, c): _*)(parameters)
  }

  def checkConcreteClass(tag: WeakTypeTag[_]): Unit = {
    val tpeSymbol = tag.tpe.typeSymbol
    if (!tpeSymbol.isClass) {
      c.abort(c.enclosingPosition, s"${tag.tpe} is not a class")
    }

    if (tpeSymbol.isAbstract) {
      c.abort(c.enclosingPosition, s"${tag.tpe} is abstract")
    }
  }

  def perspectiveFunctorK[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate[FunctorK[F]](tag)(perspectiveMapK)
  }

  def perspectiveApplyK[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate[ApplyK[F]](tag)(perspectiveMapK, map2K)
  }

  def applicativeK[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate[ApplicativeK[F]](tag)(pureK, map2K)
  }

  def foldableK[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate[FoldableK[F]](tag)(foldLeftK)
  }

  def traverseK[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate[TraverseK[F]](tag)(foldLeftK, traverseKFunc)
  }

  def distributiveK[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate[DistributiveK[F]](tag)(perspectiveMapK, cosequenceK)
  }

  def addRepresentationKToType(tag: WeakTypeTag[_])(Ta: Type): Type = {
    val algebra = tag.tpe.typeConstructor.dealias.etaExpand
    val n       = hkdSize(algebra)
    c.typecheck(tq"$Ta { type RepresentationK[A] = _root_.perspective.Finite[$n] }").tpe
  }

  def representationKInitialDecl(tag: WeakTypeTag[_])(Ta: Type): List[(Symbol, Tree)] = {
    val algebra         = tag.tpe.typeConstructor.dealias.etaExpand
    val n               = hkdSize(algebra)
    val representationK = Ta.member(TypeName("RepresentationK"))
    List((representationK, q"type RepresentationK[A] = _root_.perspective.Finite[$n]"))
  }

  def representableK[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)

    instantiate2(
      tag,
      symbolOf[RepresentableK[F]],
      addRepresentationKToType(tag),
      representationKInitialDecl(tag)
    )(indexK, tabulateK)
  }

  def allK[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate2(tag, symbolOf[All[F]], addRepresentationKToType(tag), representationKInitialDecl(tag))(
      indexK,
      tabulateK,
      foldLeftK,
      traverseKFunc
    )
  }

  //Same as instantiate but takes the symbol explicitly as it sometimes doesn't work otherwise
  //Also runs the implementation logic multiple times if for some reason Scalac changes it's mind
  def instantiate2(
      tag: WeakTypeTag[_],
      symbol: TypeSymbol,
      transformType: Type => Type = identity,
      extraInitialDeclarations: Type => List[(Symbol, Tree)] = _ => Nil
  )(
      rhs: (Type => (String, Type => Tree))*
  ): Tree = {
    val algebra = tag.tpe.typeConstructor.dealias.etaExpand
    val Ta      = transformType(appliedType(symbol, algebra))
    val impl    = rhs.map(_.apply(algebra)).toMap

    val extraDeclarations                                                 = extraInitialDeclarations(Ta)
    val extraDeclarationsSet                                              = extraDeclarations.map(_._1).toSet
    val declaration @ ClassDef(_, _, _, Template(parents, self, members)) = declareWithTypes(Ta, extraDeclarations)

    val implementations =
      for (member <- members)
        yield member match {
          case dd: DefDef if !extraDeclarationsSet(member.symbol) =>
            val method = member.symbol.asMethod
            impl
              .get(method.name.toString)
              .fold(dd)(f => defDef(method, f(method.typeSignatureIn(Ta))))

          case td: TypeDef if !extraDeclarationsSet(member.symbol) =>
            val tpe = member.symbol.asType
            impl
              .get(tpe.name.toString)
              .fold(td)(f => typeDef(tpe, f(tpe.typeSignatureIn(Ta))))

          case other => other
        }

    val definition =
      classDef(declaration.symbol, Template(parents, self, implementations))
    typeCheckWithFreshTypeParams(q"{ $definition; new ${declaration.symbol}: $Ta }"): Tree
  }

  def declareWithTypes(instance: Type, extraMembers: List[(Symbol, Tree)] = Nil): Tree = {
    val extraSymbols = extraMembers.map(_._1).toSet

    val classType = instance match {
      case RefinedType(List(tpe), _) => tpe
      case _                         => instance
    }

    val members = overridableMembersOf(instance).filter(m => m.isAbstract && !extraSymbols(m)).toSeq
    val methodStubs = delegateMethods(instance, members, NoSymbol) {
      case m => m.copy(body = q"_root_.scala.Predef.???")
    }
    val typeStubs = delegateTypes(instance, members)((tpe, args) => typeOf[Nothing]).map {
      case td: TypeDef =>
        TypeDef(Modifiers(td.mods.flags | Flag.OVERRIDE), td.name, td.tparams, td.rhs)
    }
    val stubs                       = typeStubs ++ methodStubs ++ extraMembers.map(_._2)
    val Block(List(declaration), _) = typeCheckWithFreshTypeParams(q"new $classType { ..$stubs }")
    declaration
  }

  def perspectiveFunctorKC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate[FunctorKC[F]](tag)(perspectiveMapK)
  }

  def perspectiveApplyKC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate2(tag, symbolOf[ApplyKC[F]])(perspectiveMapK, map2K)
  }

  def applicativeKC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate2(tag, symbolOf[ApplicativeKC[F]])(pureK, map2K)
  }

  def foldableKC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate[FoldableKC[F]](tag)(foldLeftK)
  }

  def traverseKC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate2(tag, symbolOf[TraverseKC[F]])(foldLeftK, traverseKFunc)
  }

  def distributiveKC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate2(tag, symbolOf[DistributiveKC[F]])(perspectiveMapK, cosequenceK)
  }

  def representableKC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate2(tag, symbolOf[RepresentableKC[F]], addRepresentationKToType(tag), representationKInitialDecl(tag))(
      indexK,
      tabulateK
    )
  }

  type AllC[F[_[_]]] = All[IgnoreC[F]#λ]

  def allKC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiate2(
      tag,
      symbolOf[AllC[F]],
      addRepresentationKToType(tag),
      representationKInitialDecl(tag)
    )(
      indexK,
      tabulateK,
      foldLeftK,
      traverseKFunc
    )
  }

  def isMaybeHKD(tpe: Type): Boolean = {
    val expanded = tpe.typeConstructor.dealias.etaExpand
    expanded.typeParams.headOption.exists(_.asType.toType.typeArgs.nonEmpty)
  }

  def instantiateHKD(tpe: Type, parents: Seq[Param])(
      constructTypes: Type*
  )(typeStep: Type => Type)(step: Seq[Param] => Tree): Tree = {
    val algebra = typeStep(tpe.typeConstructor.dealias.etaExpand)

    val params = createParameters(EmptyTree, algebra).map(_.map { param =>
      if (isMaybeHKD(param.tpe))
        instantiateHKD(param.tpe, parents :+ param)(constructTypes: _*)(typeStep)(step)
      else
        step(parents :+ param)
    })

    newClass(algebra)(constructTypes: _*)(params)
  }

  def instantiateHKDTuple(tpe: Type)(tuple1Type: Type, tuple2Type: Type, constructTypes: Type*)(
      typeStep: Type => Type
  )(
      step: Seq[Param] => (Tree, Tree)
  ): Tree = {
    val Tuple2K      = symbolOf[Tuple2K[Any, Any, Any]]
    val tupleApplied = appliedType(Tuple2K, tuple1Type, tuple2Type)
    val tupleRef     = typeRef(tupleApplied, tupleApplied.decls.head, Nil).typeConstructor.etaExpand.dealias

    instantiateHKD(tpe, Nil)(tupleRef +: constructTypes: _*)(typeStep) { params =>
      val (fst, snd) = step(params)
      q"($fst, $snd)"
    }
  }

  def namesStep(params: Seq[Param]): Tree =
    q"_root_.scala.List(..${params.map(p => q"${p.name.encodedName.toString}")})"

  def productImplicitsStep(params: Seq[Param]): Tree = {
    val last = params.last
    c.inferImplicitValue(last.tpe).orElse(abort(s"could not find implicit value of type ${last.tpe}"))
  }

  def namesType: Type = {
    val Const = c.typecheck(tq"({ type L[A] = _root_.scala.List[_root_.java.lang.String]})", c.TYPEmode).tpe
    typeRef(Const, Const.decls.head, Nil).typeConstructor.dealias.etaExpand
  }

  def names[F[_[_], _]](implicit tag: WeakTypeTag[F[Any, Any]]): Tree = {
    checkConcreteClass(tag)
    instantiateHKD(tag.tpe, Nil)(namesType, boundedWildcardType(typeBounds(typeOf[Any], typeOf[Nothing])))(identity)(
      namesStep
    )
  }

  def namesC[F[_[_]]](implicit tag: WeakTypeTag[F[Any]]): Tree = {
    checkConcreteClass(tag)
    instantiateHKD(tag.tpe, Nil)(namesType)(identity)(namesStep)
  }

  def withProductImplicits[F[_[_], _], A[_], C](
      implicit tag: WeakTypeTag[F[Any, Any]],
      aTag: WeakTypeTag[A[Any]],
      cTag: WeakTypeTag[C]
  ): Tree = {
    checkConcreteClass(tag)
    val aType = aTag.tpe.typeConstructor
    val cType = cTag.tpe

    instantiateHKD(tag.tpe, Nil)(aType, cType)(appliedType(_, aType, cType))(productImplicitsStep)
  }

  def withProductImplicitsC[F[_[_]], A[_]](implicit tag: WeakTypeTag[F[Any]], aTag: WeakTypeTag[A[Any]]): Tree = {
    checkConcreteClass(tag)
    val aType = aTag.tpe.typeConstructor

    instantiateHKD(tag.tpe, Nil)(aType)(appliedType(_, aType))(productImplicitsStep)
  }

  def namesWithProductImplicits[F[_[_], _], A[_], C](
      implicit tag: WeakTypeTag[F[Any, Any]],
      aTag: WeakTypeTag[A[Any]],
      cTag: WeakTypeTag[C]
  ): Tree = {
    checkConcreteClass(tag)
    val aType = aTag.tpe
    val cType = cTag.tpe
    instantiateHKDTuple(tag.tpe)(namesType, aType, cType)(appliedType(_, aType, cType)) { params =>
      (namesStep(params), productImplicitsStep(params))
    }
  }

  def namesWithProductImplicitsC[F[_[_]], A[_]](implicit tag: WeakTypeTag[F[Any]], aTag: WeakTypeTag[A[Any]]): Tree = {
    checkConcreteClass(tag)
    val aType = aTag.tpe
    instantiateHKDTuple(tag.tpe)(namesType, aType)(appliedType(_, aType)) { params =>
      (namesStep(params), productImplicitsStep(params))
    }
  }
}
