package perspective.derivation

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait HKDSumGenericMacros {

  implicit def materializeHKDSum[A]: HKDSumGeneric[A] =
    macro HKDSumGenericMacrosImpl.materializeHKDSum[A]
}
class HKDSumGenericMacrosImpl(val c: whitebox.Context) {
  import c.universe._

  def materializeHKDSum[A: WeakTypeTag]: Tree = {
    val tpe    = weakTypeOf[A]
    val tpeSym = tpe.typeSymbol

    if (tpe =:= typeOf[AnyRef] || !tpeSym.isAbstract) {
      c.abort(c.enclosingPosition, s"$tpe is not a valid sum type")
    }

    val classTpeSym = tpeSym.asClass
    val subclasses  = classTpeSym.knownDirectSubclasses.map(_.asClass).toSeq
    val n           = subclasses.size

    if (subclasses.isEmpty) {
      c.abort(c.enclosingPosition, s"Not subclasses found for $tpe")
    }

    val names = subclasses.map(c => q"${c.fullName}").toList
    val types = subclasses.map(_.asType.toType)

    val string = typeOf[String]
    
    val indexMapValues = names.zipWithIndex.map { case (name, idx) =>
      q"($name, _root_.perspective.Finite($idx))"
    }

    if (n > 99) {
      c.abort(c.enclosingPosition, "perspective does not support classes with more than 99 fields")
    } else if (n > 22) {
      q"""
        new _root_.perspective.derivation.HKDSumGeneric[$tpe] {
          import _root_.scala.collection.immutable.ArraySeq
          import _root_.perspective.derivation.ArrayProductK
          
          private def makeG[F[_]](product: ArrayProductK[F, $n]): Gen[F] = product.asInstanceOf[Gen[F]]
          private def fromG[F[_]](product: Gen[F]): ArrayProductK[F, $n] = product.asInstanceOf[ArrayProductK[F, $n]]
          
          override type Gen[A[_]] = ArrayProductK.NewTypes.${TypeName(s"G$n")}[A, ..$types]
          
          type Index[X] = _root_.perspective.Finite[$n]
          
          override def typeName: $string = ${tpeSym.fullName}
          
          override def names: Gen[({type L[A] = $string})#L] =
            makeG[({type L[A] = $string})#L](ArrayProductK[({type L[A] = $string})#L, $n](ArraySeq(..$names)))
          
          override def nameToIndexMap: _root_.scala.Predef.Map[String, Index[_ <: $tpe]] =
            _root_.scala.Predef.Map(..$indexMapValues)
            
          override def indexToNameMap: _root_.scala.Predef.Map[Index[_ <: $tpe], String] =
            nameToIndexMap.map(_.swap)
          
          override def indexOf[X <: $tpe](x: X): Index[X] = x match {
            case ..${types.zipWithIndex.map { case (tpe, i) => cq"_: $tpe => _root_.perspective.Finite($i)" }}
          }
          
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
        new _root_.perspective.derivation.HKDSumGeneric[$tpe] {
          override type Gen[A[_]] = _root_.perspective.derivation.${TypeName("Product" + n + "K")}[A, ..$types]
          
          type Index[X] = _root_.perspective.Finite[$n]
          
          override def typeName: $string = ${tpeSym.fullName}
          
          override def names: Gen[({type L[A] = $string})#L] =
            ${constructGen(tq"({type L[A] = $string})#L", names)}
          
          override def nameToIndexMap: _root_.scala.Predef.Map[String, Index[_ <: $tpe]] =
            _root_.scala.Predef.Map(..$indexMapValues)
            
          override def indexToNameMap: _root_.scala.Predef.Map[Index[_ <: $tpe], String] =
            nameToIndexMap.map(_.swap)
          
          override def indexOf[X <: $tpe](x: X): Index[X] = x match {
            case ..${types.zipWithIndex.map { case (tpe, i) => cq"_: $tpe => _root_.perspective.Finite($i)" }}
          }
          
          //Need cast to hide the index type
          override val representable: _root_.perspective.RepresentableKC.Aux[Gen, Index] =
            $productKTermName.${TermName(s"product${n}KRepresentableTraverseInstance")}.asInstanceOf[_root_.perspective.RepresentableKC.Aux[Gen, Index]]
          override val traverse: _root_.perspective.TraverseKC[Gen] =
            $productKTermName.${TermName(s"product${n}KRepresentableTraverseInstance")}
        }"""
    }
  }
}
