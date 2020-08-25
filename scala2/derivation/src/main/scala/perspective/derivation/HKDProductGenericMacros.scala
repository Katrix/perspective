package perspective.derivation

import scala.language.experimental.macros
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
    val paramss = classTpeSym.primaryConstructor.asMethod.paramLists
    val flatParams = paramss.flatten
    val n = flatParams.length

    if(n > 22) {
      c.abort(c.enclosingPosition, "Case classes of size bigger than 22 not yet supported")
    }

    for {
      param  <- flatParams
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
    val fromValues = paramss.foldLeft((1, Nil: List[List[Tree]])) {
      case ((n, acc), params) =>
        val (n2, acc2) = params.foldLeft((n, Nil: List[Tree])) {
          case ((n, acc), _) => (n + 1) -> (q"a.${TermName(s"p$n")}" :: acc)
        }

        n2 -> (acc2.reverse :: acc)
    }._2.reverse

    val string = typeOf[String]

    val productKTermName = q"_root_.perspective.derivation.${TermName("Product" + n + "K")}"

    def constructGen(hkType: Tree, params: List[Tree]): Tree =
      q"$productKTermName[$hkType, ..$types](..$params)"

    def constructObj(paramss: List[List[Tree]]) =
      q"new $classTpeSym(...$paramss)"

    q"""new _root_.perspective.derivation.HKDProductGeneric[$tpe] {
         override type Gen[A[_]] = _root_.perspective.derivation.${TypeName("Product" + n + "K")}[A, ..$types]
         
         override def typeName: $string = ${tpeSym.name.decodedName.toString}

         override def names: Gen[({type L[A] = _root_.perspective.Const[$string, A]})#L] =
          ${constructGen(tq"({type L[A] = _root_.perspective.Const[$string, A]})#L", names)}
         
         override def to(a: $tpe): Gen[_root_.cats.Id] = ${constructGen(tq"({type L[A] = A})#L", toValues)}
         override def from(a: Gen[_root_.cats.Id]): $tpe = ${constructObj(fromValues)}

         override def representable: _root_.perspective.RepresentableKC[Gen] =
           $productKTermName.${TermName(s"product${n}KRepresentableTraverseInstance")}
         override def traverse: _root_.perspective.TraverseKC[Gen] =
           $productKTermName.${TermName(s"product${n}KRepresentableTraverseInstance")}
       }"""
  }
}
