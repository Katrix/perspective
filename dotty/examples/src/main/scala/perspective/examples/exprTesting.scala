package perspective.examples

import scala.deriving.Mirror
import scala.quoted.*

import io.circe.*
import perspective._
import perspective.derivation.ExprHKDProductGeneric

trait PerspectiveExprEncoder[A] extends Encoder[A]
object PerspectiveExprEncoder:
  inline def deriveProductEncoder[A]: PerspectiveExprEncoder[A] =
    ${ deriveProductEncoderImpl[A] }

  def deriveProductEncoderImpl[A: Type](using q: Quotes): Expr[PerspectiveExprEncoder[A]] =
    import q.reflect.*
    val gen   = ExprHKDProductGeneric.derived[A]
    val types = gen.types
    val names = gen.names.asInstanceOf[gen.Gen[Const[String]]]
    //TODO: Make a function that errors if instances are not found
    val instances = gen.summonInstancesOpt[Encoder].getOrElse(report.errorAndAbort("Missing implicit instances"))

    val res = '{
      new PerspectiveExprEncoder[A] {
        override def apply(a: A): Json = Json.obj(${
          val repr = gen.to('a)
          val res = gen.tabulateConst(
            [X] =>
              (idx: gen.Index[X]) =>
                TypeRepr.of(using gen.indexK(types)(idx)) match {
                  case t if t <:< TypeRepr.of[Byte] =>
                    (gen.indexK(names)(idx), '{ Json.fromInt(${ gen.indexK(repr)(idx).asExprOf[Byte] }.toInt) })
                  case t if t <:< TypeRepr.of[Char] =>
                    (gen.indexK(names)(idx), '{ Json.fromString(${ gen.indexK(repr)(idx).asExprOf[Char] }.toString) })
                  case t if t <:< TypeRepr.of[Short] =>
                    (gen.indexK(names)(idx), '{ Json.fromInt(${ gen.indexK(repr)(idx).asExprOf[Short] }.toInt) })
                  case t if t <:< TypeRepr.of[Int] =>
                    (gen.indexK(names)(idx), '{ Json.fromInt(${ gen.indexK(repr)(idx).asExprOf[Int] }) })
                  case t if t <:< TypeRepr.of[Long] =>
                    (gen.indexK(names)(idx), '{ Json.fromLong(${ gen.indexK(repr)(idx).asExprOf[Long] }) })
                  case t if t <:< TypeRepr.of[Float] =>
                    (gen.indexK(names)(idx), '{ Json.fromFloatOrString(${ gen.indexK(repr)(idx).asExprOf[Float] }) })
                  case t if t <:< TypeRepr.of[Double] =>
                    (gen.indexK(names)(idx), '{ Json.fromDoubleOrString(${ gen.indexK(repr)(idx).asExprOf[Double] }) })
                  case t if t <:< TypeRepr.of[Boolean] =>
                    (gen.indexK(names)(idx), '{ Json.fromBoolean(${ gen.indexK(repr)(idx).asExprOf[Boolean] }) })
                  case t if t <:< TypeRepr.of[String] =>
                    (gen.indexK(names)(idx), '{ Json.fromString(${ gen.indexK(repr)(idx).asExprOf[String] }) })
                  case _ =>
                    given Type[X] = gen.indexK(types)(idx)
                    (gen.indexK(names)(idx), '{ ${ gen.indexK(instances)(idx) }.apply(${ gen.indexK(repr)(idx) }) })
              }
          )
          val l = gen.toListK(res).map(t => '{ (${ Expr(t._1) }, ${ t._2 }) })
          Expr.ofSeq(l)
        }: _*)
      }
    }
    res
