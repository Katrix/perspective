package perspective.macros

import scala.reflect.macros.whitebox

private[perspective] class PerspectiveAnnotationMacros(override val c: whitebox.Context) extends DeriveMacros(c) {
  import c.universe._

  def hkd(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val (rawClassDef, optCompanionDef) = annottees match {
      case (clazz: ClassDef) :: (companion: ModuleDef) :: Nil => clazz -> Some(companion)
      case (clazz: ClassDef) :: Nil                           => clazz -> None
      case _                                                  => c.abort(c.enclosingPosition, "@hkd can only be used on classes")
    }
    val classDef = c.typecheck(rawClassDef).asInstanceOf[ClassDef]

    val typeName = classDef.name
    val tpe = classDef.symbol.asType.toType

    val moduleDef = optCompanionDef.getOrElse(q"object ${classDef.name.toTermName}": ModuleDef)

    if (!isMaybeHKD(tpe)) {
      c.abort(c.enclosingPosition, "@hkd can only be used on higher kinded data types")
    }

    val n = hkdSize(tpe)

    val deriveAll = {
      val implicitsType = tq"_root_.perspective.TraverseKC[$typeName] with _root_.perspective.RepresentableKC[$typeName] { type RepresentationK[_] = _root_.perspective.Finite[$n] }"
      q"""implicit val ${TermName(c.freshName("hkdInstances"))}: $implicitsType = 
            _root_.perspective.macros.Derive.allKC[$typeName, ({type L[A] = _root_.perspective.Finite[$n]})#L]"""
    }

    val gatherImplicits = {
      val classTypes = createParameters(EmptyTree, tpe).zip(hkdParamSize(tpe)).map(t => t._1.zip(t._2).map {
        case (p, 1) => 
          val innerType = p.tpe match {
            case TypeRef(_, _, List(t)) => t
          }
          
          tq"F[$innerType]"
        case (p, _) =>
          val outerType = p.tpe match {
            case TypeRef(_, t, _) => t
          }
          
          tq"$outerType[F]"
      })
      val argValues = allParams(tpe).map(_.map(_ => TermName(c.freshName())))

      val parameters = argValues.zip(classTypes).flatMap {
        case (args, types) =>
          args.zip(types).map { case (arg, tpe) => q"$arg: $tpe" }
      }

      q"""implicit def ${TermName(c.freshName("gatherImplicits"))}[F[_]](
            implicit ..$parameters
          ): $typeName[F] = new $typeName[F](...$argValues)"""
    }

    val names =
      q"lazy val names: $typeName[({type L[A] = List[String]})#L] = _root_.perspective.macros.Derive.namesC[$typeName]"

    val newModuleDef = ModuleDef(
      moduleDef.mods,
      moduleDef.name,
      Template(
        moduleDef.impl.parents,
        moduleDef.impl.self,
        List(deriveAll, gatherImplicits, names) ++ moduleDef.impl.body
      )
    )

    Block(List(rawClassDef, newModuleDef), Literal(Constant(())))
  }
}
