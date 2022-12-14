package perspective.derivation

import scala.language.experimental.macros

import scala.collection.immutable.ArraySeq
import scala.reflect.macros.whitebox

trait HKDProductGenericMacros {

  implicit def materializeHKDProduct[A <: Product]: HKDProductGeneric[A] =
    macro HKDProductGenericMacrosImpl.materializeHKDProduct[A]
}

class HKDProductGenericMacrosImpl(val c: whitebox.Context) {
  import c.universe._

  def materializeHKDProduct[A <: Product: WeakTypeTag]: Tree = {
    val tpe    = weakTypeOf[A]
    val tpeSym = tpe.typeSymbol

    if (tpe =:= typeOf[AnyRef] || !tpeSym.isClass || !tpeSym.asClass.isCaseClass) {
      c.abort(c.enclosingPosition, s"$tpe is not a valid product type")
    }

    val classTpeSym = tpeSym.asClass
    val paramss     = classTpeSym.primaryConstructor.asMethod.paramLists
    val flatParams  = paramss.flatten
    val n           = flatParams.length

    for {
      param <- flatParams
    } {
      val field = tpe.decl(param.name)

      if (field == NoSymbol) {
        c.abort(c.enclosingPosition, s"No field for ${param.fullName} found")
      }

      if (!field.asMethod.isAccessor) {
        c.abort(c.enclosingPosition, s"No accessor for ${param.fullName} found")
      }
    }

    val names = flatParams.map(p => q"${p.name.decodedName.toString}")
    val types = flatParams.map(p => tpe.decl(p.name).typeSignatureIn(tpe).finalResultType)

    val toValues = flatParams.map(p => q"a.${p.name.toTermName}")
    val fromValues = paramss
      .foldLeft((1, Nil: List[List[Tree]])) { case ((n, acc), params) =>
        val (n2, acc2) = params.foldLeft((n, Nil: List[Tree])) { case ((n, acc), _) =>
          (n + 1) -> (q"a.${TermName(s"p$n")}" :: acc)
        }

        n2 -> (acc2.reverse :: acc)
      }
      ._2
      .reverse

    val string = typeOf[String]

    def constructObj(paramss: List[List[Tree]]) =
      q"new $classTpeSym(...$paramss)"

    if (n > 99) {
      c.abort(c.enclosingPosition, "perspective does not support classes with more than 99 fields")
    } else if (n > 22) {
      val fromValuesArr = paramss
        .foldLeft((1, types, Nil: List[List[Tree]])) { case ((n, topRemainingTypes, acc), params) =>
          val (n2, newRemainingTypes, acc2) = params.foldLeft((n, topRemainingTypes, Nil: List[Tree])) {
            case ((n, remainingTypes, acc), _) =>
              (n + 1, remainingTypes.tail, q"aArr(${n - 1}).asInstanceOf[${remainingTypes.head}]" :: acc)
          }

          (n2, newRemainingTypes, acc2.reverse :: acc)
        }
        ._3
        .reverse

      q"""
        new _root_.perspective.derivation.HKDProductGeneric[$tpe] {
          import _root_.scala.collection.immutable.ArraySeq
          import _root_.perspective.derivation.ArrayProductK
          import _root_.perspective.{~>:, Finite}

          private def makeG[F[_]](product: ArrayProductK[F, $n]): Gen[F] = product.asInstanceOf[Gen[F]]
          private def fromG[F[_]](product: Gen[F]): ArrayProductK[F, $n] = product.asInstanceOf[ArrayProductK[F, $n]]
          
          override type Gen[A[_]] = ArrayProductK.NewTypes.${TypeName(s"G$n")}[A, ..$types]

          override type Index[A] = _root_.perspective.Finite[$n]
          
          override def typeName: $string = ${tpeSym.fullName}
          
          override def names: Gen[({type L[A] = $string})#L] =
            makeG[({type L[A] = $string})#L](ArrayProductK[({type L[A] = $string})#L, $n](ArraySeq(..$names)))
          
          override def to(a: $tpe): Gen[_root_.cats.Id] = 
            makeG[({type L[A] = A})#L](ArrayProductK[({type L[A] = A})#L, $n](ArraySeq(..$toValues)))
          override def from(a: Gen[_root_.cats.Id]): $tpe = {
            val aArr = fromG(a).arr
            ${constructObj(fromValuesArr)}
          }

          override def tabulateFoldLeft[B](start: B)(f: B => Index ~>: ({type L[X] = B})#L): B = {
            var i: Int = 0
            var res: B = start
            while (i < $n) {
              res = f(res)(Finite.applyUnsage[$n](i))
              i += 1
            }
            res
          }

          override def tabulateTraverseK[G[_], B[_]](f: Index ~>: ({type L[X] = G[B[X]]})#L)(implicit G: _root_.cats.Applicative[G]): G[Gen[B]] = {
            var acc: G[List[B[Any]]] = G.pure(List.empty[B[Any]])
            var i: Int               = 0
            while (i < $n) {
              acc = G.map2(f[Any](Finite.applyUnsage[$n](i)), acc)((v, a) => v :: a)
              i += 1
            }
     
            G.map(acc) { a =>
              makeG(ArrayProductK[B, $n](ArraySeq.from(a: List[Any]).asInstanceOf[ArraySeq[B[Any]]]))
            }
          }

          override def tabulateTraverseKOption[B[_]](f: Index ~>: ({type L[X] = Option[B[X]]})#L): Option[Gen[B]] = {
            val arr    = new Array[Any]($n).asInstanceOf[Array[B[Any]]]
            var i: Int = 0
            while (i < $n) {
              val res = f[Any](Finite.applyUnsage[$n](i))
          
              if (res.isEmpty)
                // Early return
                return None
              else arr(i) = res.get
          
              i += 1
            }

            Some(makeG(ArrayProductK[B, $n](ArraySeq.unsafeWrapArray(arr))))
          }

          override def tabulateTraverseKEither[E, B[_]](f: Index ~>: ({type L[X] = Either[E, B[X]]})#L): Either[E, Gen[B]] = {
            val arr    = new Array[Any]($n).asInstanceOf[Array[B[Any]]]
            var i: Int = 0
            while (i < $n) {
              val res = f[Any](Finite.applyUnsage[$n](i))
              res match {
                case Right(v) =>
                  arr(i) = v
                case Left(e) =>
                  // Early return
                  return Left(e)
              }
              i += 1
            }
            
            Right(makeG(ArrayProductK[B, $n](ArraySeq.unsafeWrapArray(arr))))
          }

          override def productElementId[X](a: $tpe, index: Index[X]): X =
            a.productElement(index.value).asInstanceOf[X]
          
          private val instance: _root_.perspective.RepresentableKC.Aux[Gen, Index] with _root_.perspective.TraverseKC[Gen] =
            ArrayProductK.instance[$n].asInstanceOf[_root_.perspective.RepresentableKC.Aux[Gen, Index] with _root_.perspective.TraverseKC[Gen]]
          
          override val representable: _root_.perspective.RepresentableKC.Aux[Gen, Index] = instance
          override val traverse: _root_.perspective.TraverseKC[Gen] = instance
        }"""
    } else {
      val productKTermName = q"_root_.perspective.derivation.${TermName("Product" + n + "K")}"

      def constructGen(hkType: Tree, params: List[Tree]): Tree =
        q"$productKTermName[$hkType, ..$types](..$params)"

      q"""
        new _root_.perspective.derivation.HKDProductGeneric[$tpe] {
          import _root_.perspective.{~>:, Finite}

          override type Gen[A[_]] = _root_.perspective.derivation.${TypeName("Product" + n + "K")}[A, ..$types]

          override type Index[A] = _root_.perspective.Finite[$n]

          override def typeName: $string = ${tpeSym.fullName}

          override def names: Gen[({type L[A] = $string})#L] =
            ${constructGen(tq"({type L[A] = $string})#L", names)}
         
          override def to(a: $tpe): Gen[_root_.cats.Id] = ${constructGen(tq"({type L[A] = A})#L", toValues)}
          override def from(a: Gen[_root_.cats.Id]): $tpe = ${constructObj(fromValues)}

          override def tabulateFoldLeft[B](start: B)(f: B => Index ~>: ({type L[X] = B})#L): B = {
            var i: Int = 0
            var res: B = start
            while (i < $n) {
              res = f(res)(Finite.applyUnsage[$n](i))
              i += 1
            }
            res
          }

          override def tabulateTraverseK[G[_], B[_]](f: Index ~>: ({type L[X] = G[B[X]]})#L)(implicit G: _root_.cats.Applicative[G]): G[Gen[B]] = {
            var acc: G[List[B[Any]]] = G.pure(List.empty[B[Any]])
            var i: Int               = 0
            while (i < $n) {
              acc = G.map2(f[Any](Finite.applyUnsage[$n](i)), acc)((v, a) => v :: a)
              i += 1
            }

            G.map(acc) { a =>
              val vec = a.toIndexedSeq
              ${constructGen(tq"B", types.zipWithIndex.map(t => q"vec(${t._2}).asInstanceOf[B[${t._1}]]"))}
            }
          }

          override def tabulateTraverseKOption[B[_]](f: Index ~>: ({type L[X] = Option[B[X]]})#L): Option[Gen[B]] = {
            val arr    = new Array[Any]($n)
            var i: Int = 0
            while (i < $n) {
              val res = f[Any](Finite.applyUnsage[$n](i))

              if (res.isEmpty)
                // Early return
                return None
              else arr(i) = res.get.asInstanceOf[Any]

              i += 1
            }

            Some(${constructGen(tq"B", types.zipWithIndex.map(t => q"arr(${t._2}).asInstanceOf[B[${t._1}]]"))})
          }

          override def tabulateTraverseKEither[E, B[_]](f: Index ~>: ({type L[X] = Either[E, B[X]]})#L): Either[E, Gen[B]] = {
            val arr    = new Array[Any]($n)
            var i: Int = 0
            while (i < $n) {
              val res = f[Any](Finite.applyUnsage[$n](i))
              res match {
                case Right(v) =>
                  arr(i) = v.asInstanceOf[Any]
                case Left(e) =>
                  // Early return
                  return Left(e)
              }
              i += 1
            }

            Right(${constructGen(tq"B", types.zipWithIndex.map(t => q"arr(${t._2}).asInstanceOf[B[${t._1}]]"))})
          }

          override def productElementId[X](a: $tpe, index: Index[X]): X =
            a.productElement(index.value).asInstanceOf[X]

          override val representable: _root_.perspective.RepresentableKC.Aux[Gen, Index] =
            $productKTermName.${TermName(s"product${n}KRepresentableTraverseInstance")}
          override val traverse: _root_.perspective.TraverseKC[Gen] =
            $productKTermName.${TermName(s"product${n}KRepresentableTraverseInstance")}
        }"""
    }
  }
}
