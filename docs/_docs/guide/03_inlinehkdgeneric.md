---
title: InlineHKDGeneric 
---

# {{page.title}}

`perspective` provides some special instances of `HKDGeneric` for other or more specialized
purposes. `InlineHKDGeneric` is one example here, and provides a variant of `HKDGeneric` to be used
in inline functions, and generate nicer and often faster bytecode.

`InlineHKDGeneric` reimplements all the functions defined in typeclasses on `InlineHKDGeneric`
itself.

InlineHKDProductGeneric uses a lot of beta reduction to simplify code and avoid lambdas in the
generated code. At the time of writing InlineHKDGeneric, this beta reduction did not work with
polymorphic functions. perspective gets around this by representing the `Index` type differently
using path dependent types. Because of this, use of `tabulate` functions is heavily recommended when
using `InlineHKDGeneric` Non-tabulate functions are still left in the API but might not generate as
good bytecode.

## Implicits

Implicit instances of `Gen[F]` is gotten using the function `gen.summonInstances[F]` when working
with `InlineHKDGeneric`.

## Unrolling

`InlineHKDGeneric` can in some cases perform loop unrolling if instructed to. Some `tabulate`
functions have another paramete `unroll: Boolean = false`. Setting this parameter to true will
unroll the loop perspective generates. When doing loop unrolling, the code can be specialized based
on the current type being worked on. To do this, two extra functions are
used, `productElementIdExact` and `lateInlineMatch `. `productElementIdExact` works
like `productElementId` but refines the type when used within an unrolled block. `lateInlineMatch`
is like an `inline match`, but expands slightly later, after the type of `productElementIdExact` has
been refined.

### Example

```scala 3 sc:nocompile
trait PerspectiveInlineEncoder[A] extends Encoder[A]
object PerspectiveInlineEncoder:

  inline def derivedProductEncoder[A](using gen: InlineHKDProductGeneric[A]): PerspectiveInlineEncoder[A] =
    new PerspectiveInlineEncoder[A]:
      private val names    = gen.names
      private val encoders = gen.summonInstances[Encoder]

      override def apply(a: A): Json =
        val list = gen.tabulateFoldLeft(Nil: List[(String, Json)], unrolling = true)((acc, idx) =>
          val js = gen.lateInlineMatch {
            a.productElementIdExact(idx) match {
              case p: Byte    => Json.fromInt(p)
              case p: Char    => Json.fromString(p.toString)
              case p: Short   => Json.fromInt(p)
              case p: Int     => Json.fromInt(p)
              case p: Long    => Json.fromLong(p)
              case p: Float   => Json.fromFloatOrString(p)
              case p: Double  => Json.fromDoubleOrString(p)
              case p: Boolean => Json.fromBoolean(p)
              case p: String  => Json.fromString(p)
              case other      => encoders.indexK(idx)(other)
            }
          }

          (names.indexK(idx), js) :: acc
        )

        Json.obj(list: _*)

  inline def derived[A](using gen: InlineHKDGeneric[A]): PerspectiveInlineEncoder[A] = inline gen match
    case gen: InlineHKDProductGeneric.Aux[A, gen.Gen] => derivedProductEncoder(using gen)
```

