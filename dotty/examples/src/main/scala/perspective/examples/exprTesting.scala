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
    given gen: ExprHKDProductGeneric[A] = ExprHKDProductGeneric.derived[A]

    val types = gen.types
    val names = gen.names.asInstanceOf[gen.Gen[Const[String]]]
    // TODO: Make a function that errors if instances are not found
    val instances = gen.summonInstancesOpt[Encoder].getOrElse(report.errorAndAbort("Missing implicit instances"))

    '{
      new PerspectiveExprEncoder[A]:
        override def apply(a: A): Json = Json.obj(${
          val repr = gen.to('a)
          val res = gen.tabulateConst {
            [X] =>
              (idx: gen.Index[X]) =>
                val name  = names.indexK(idx)
                val value = repr.indexK(idx)

                given Type[X] = types.indexK(idx)

                val encoded = TypeRepr.of[X] match {
                  case t if t <:< TypeRepr.of[Byte] =>
                    '{ Json.fromInt(${ value.asExprOf[Byte] }.toInt) }
                  case t if t <:< TypeRepr.of[Char] =>
                    '{ Json.fromString(${ value.asExprOf[Char] }.toString) }
                  case t if t <:< TypeRepr.of[Short] =>
                    '{ Json.fromInt(${ value.asExprOf[Short] }.toInt) }
                  case t if t <:< TypeRepr.of[Int] =>
                    '{ Json.fromInt(${ value.asExprOf[Int] }) }
                  case t if t <:< TypeRepr.of[Long] =>
                    '{ Json.fromLong(${ value.asExprOf[Long] }) }
                  case t if t <:< TypeRepr.of[Float] =>
                    '{ Json.fromFloatOrString(${ value.asExprOf[Float] }) }
                  case t if t <:< TypeRepr.of[Double] =>
                    '{ Json.fromDoubleOrString(${ value.asExprOf[Double] }) }
                  case t if t <:< TypeRepr.of[Boolean] =>
                    '{ Json.fromBoolean(${ value.asExprOf[Boolean] }) }
                  case t if t <:< TypeRepr.of[String] =>
                    '{ Json.fromString(${ value.asExprOf[String] }) }
                  case _ =>
                    '{ ${ instances.indexK(idx) }.apply(${ value }) }
                }
                '{ (${ Expr(name) }, ${ encoded }) }
          }
          Expr.ofSeq(res.toListK)
        }: _*)
    }
