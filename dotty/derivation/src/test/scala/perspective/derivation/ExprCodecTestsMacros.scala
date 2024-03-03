package perspective.derivation

import scala.deriving.Mirror
import scala.quoted.*

import cats.Monad
import io.circe.*
import io.circe.syntax.*
import org.scalatest.funsuite.AnyFunSuite
import perspective.*
import perspective.derivation.{ExprHKDProductGeneric, HKDProductGeneric}

object ExprCodecTestsMacros {
  trait PerspectiveExprEncoder[A] extends Encoder[A]
  object PerspectiveExprEncoder:
    inline def derived[A]: PerspectiveExprEncoder[A] =
      ${ deriveProductEncoderImpl[A] }

    def deriveProductEncoderImpl[A: Type](using q: Quotes): Expr[PerspectiveExprEncoder[A]] =
      import q.reflect.*
      given gen: ExprHKDProductGeneric[A] = ExprHKDProductGeneric.derived[A]

      val types     = gen.types
      val names     = gen.names.asInstanceOf[gen.Gen[Const[String]]]
      val instances = gen.summonInstances[Encoder]

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

  trait PerspectiveExprDecoder[A] extends Decoder[A]
  object PerspectiveExprDecoder:
    inline def derived[A]: PerspectiveExprDecoder[A] =
      ${ deriveProductDecoderImpl[A] }

    def deriveProductDecoderImpl[A: Type](using q: Quotes): Expr[PerspectiveExprDecoder[A]] =
      import q.reflect.*
      given gen: ExprHKDProductGeneric[A] = ExprHKDProductGeneric.derived[A]

      val types     = gen.types
      val names     = gen.names.asInstanceOf[gen.Gen[Const[String]]]
      val instances = gen.summonInstances[Decoder]

      '{
        new PerspectiveExprDecoder[A]:
          override def apply(c: HCursor): Decoder.Result[A] =
            ${
              gen.tabulateMatchExprEither[DecodingFailure, Id, A](using q)(
                [X] =>
                  (idx: gen.Index[X]) =>
                    given Type[X] = types.indexK(idx)

                    '{ c.get[X](${ Expr(names.indexK(idx)) })(${ instances.indexK(idx) }) }
                ,
                genV => gen.from(genV)
              )
            }
      }
}
