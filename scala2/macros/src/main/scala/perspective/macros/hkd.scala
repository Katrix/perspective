package perspective.macros

import scala.language.experimental.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}

@compileTimeOnly("Cannot expand @hkd")
class hkd extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro PerspectiveAnnotationMacros.hkd
}
