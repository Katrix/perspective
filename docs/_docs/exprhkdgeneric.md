---
title: HKDGeneric 
---

# {{page.title}}

`ExprHKDGeneric` is a version of `HKDGeneric` for use in macros. Its `to` and `from` functions have
types `def to(a: Expr[A]): Gen[Expr]` and `def from(gen: Gen[Expr]): Expr[A]`. `ExprHKDGeneric` also
has functions `def types: Gen[Type]` and `def genType: Type[Gen]`.

## Traverse

Traverse is problematic for macros, and a true traverse function is incompatible with `Expr`.
perspective provides a lot of traverse like functions to try to fill this gap. Sadly, none of these
are truly traverse. In some cases a fold function can be used instead, to get the results of the traverse.

```scala 3 sc:nocompile
def tabulateTraverseKExprId[B[_] : Type](f: Index :~>: Compose2[Expr, B]): Expr[Gen[B]]

def tabulateTraverseKExpr[B[_] : Type, D[_] : Type](
  f: Index :~>: Compose3[Expr, B, D],
  BAppExpr: Expr[Applicative[B]]
): Expr[B[Gen[D]]]

// Below only for product types
def tabulateFlatMappableExpr[B[_] : Type, D[_] : Type, R: Type](using q: Quotes)(
  f: Index :~>: Compose3[Expr, B, D],
  transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R],
  extractFlatMap: Quotes ?=> [X] => (Expr[B[D[X]]], Index[X], Quotes ?=> Expr[D[X]] => Expr[B[R]]) => Expr[B[R]],
  extractMap: Quotes ?=> [X] => (Expr[B[D[X]]], Index[X], Quotes ?=> Expr[D[X]] => Expr[R]) => Expr[B[R]]
): Expr[B[R]]

def tabulateFlatMapExpr[B[_] : Type, D[_] : Type, R: Type](using q: Quotes)(
  f: Index :~>: Compose3[Expr, B, D],
  transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R],
  BFlatMapExpr: Expr[Monad[B]]
): Expr[B[R]]

def tabulateMatchExprOption[D[_] : Type, R: Type](using q: Quotes)(
  f: Index :~>: Compose3[Expr, Option, D],
  transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R]
): Expr[Option[R]]

def tabulateMatchExprEither[E: Type, D[_] : Type, R: Type](using q: Quotes)(
  f: Index :~>: Compose3[Expr, [X] =>> Either[E, X], D],
  transform: Quotes ?=> Gen[Compose2[Expr, D]] => Expr[R]
): Expr[Either[E, R]]
```

## Example

```scala 3 sc:nocompile
trait PerspectiveExprEncoder[A] extends Encoder[A]

object PerspectiveExprEncoder:
  inline def deriveProductEncoder[A]: PerspectiveExprEncoder[A] =
    ${ deriveProductEncoderImpl[A] }

  def deriveProductEncoderImpl[A: Type](using q: Quotes): Expr[PerspectiveExprEncoder[A]] =
    import q.reflect.*
    given gen: ExprHKDProductGeneric[A] = ExprHKDProductGeneric.derived[A]

    val types = gen.types
    val names = gen.names.asInstanceOf[gen.Gen[Const[String]]]
    val instances = gen.summonInstances[Encoder]

    '{
      new PerspectiveExprEncoder[A]:
        override def apply(a: A): Json = Json.obj(${
          val repr = gen.to('a)
          val res = gen.tabulateConst {
            [X] =>
              (idx: gen.Index[X]) =>
                val name = names.indexK(idx)
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
            Expr . ofSeq(res.toListK)
        }: _*)
    }

trait PerspectiveExprDecoder[A] extends Decoder[A]

object PerspectiveExprDecoder:
  inline def deriveProductDecoder[A]: PerspectiveExprDecoder[A] =
    ${ deriveProductDecoderImpl[A] }

  def deriveProductDecoderImpl[A: Type](using q: Quotes): Expr[PerspectiveExprDecoder[A]] =
    import q.reflect.*
    given gen: ExprHKDProductGeneric[A] = ExprHKDProductGeneric.derived[A]

    val types = gen.types
    val names = gen.names.asInstanceOf[gen.Gen[Const[String]]]
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
```