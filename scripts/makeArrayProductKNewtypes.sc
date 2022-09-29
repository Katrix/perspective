import ammonite.ops._

@main def main(min: Int = 23, max: Int) = {
  val entries = (min to max).map { n =>
    val types    = (1 to n).map(i => s"T$i")
    val typesStr = types.mkString(", ")
    s"""|type G$n[F[_], $typesStr] <: G$n.Base with Tag
        |object G$n {
        |  trait Base
        |  implicit def gatherImplicitsG$n[F[_], $typesStr](implicit
        |    ${types.zipWithIndex.map(t => s"p${t._2 + 1}: F[${t._1}]").mkString(", ")}
        |  ): G$n[F, $typesStr] = ArrayProductK[F, $n](ArraySeq(${(1 to n)
         .map(i => s"p$i")
         .mkString(", ")})).asInstanceOf[G$n[F, $typesStr]]
        |}""".stripMargin
  }

  val fileContent =
    s"""|package perspective.derivation
        |
        |import scala.collection.immutable.ArraySeq
        |
        |trait ArrayProductKNewtypes {
        |  trait Tag extends Any
        |  // format: off
        |
        |${entries.map(s => s.split('\n').map("  " + _).mkString("\n")).mkString("\n\n")}
        |
        |  // format: on
        |}""".stripMargin

  write(pwd / "ArrayProductKNewtypes.scala", fileContent)
}
