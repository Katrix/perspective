package perspective.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

@compileTimeOnly("Cannot expand @hkd")
class hkd extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro PerspectiveAnnotationMacros.hkd
}
