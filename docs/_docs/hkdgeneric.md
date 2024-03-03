---
title: HKDGeneric 
---

# {{page.title}}

`HKDGeneric[A]` is a typeclass that provides functions to convert `A` to and from some higher kinded
data type `Gen`. These functions are `def to(a: A): Gen[Id]` and `def from(gen: Gen[Id]): A`. The
type `Gen` can be manipulated and changed in the same way as a `List` can be changed. Functions
providing these operations are exposed on higher kinded versions of typical typeclasses like
`Functor`, `Applicative`, `Traverse` and so on. More details on these operations and typeclases can
be found in [typeclasses](typeclasses.md). Of particular note is `Representable`

## "Fused" `RepresentableK` operations

As it is so central to what `HKDGeneric` does, `HKDGeneric` exposes "fused" functions,
combining `RepresentableK.indicesK` with something some other functions, like `foldLeftK`
and `traverseK` for performance.

## HKDGeneric and implicits

The higher kinded representation `Gen` from `HKDGeneric` provides implicit instances of `Gen[F]` if
all the types making up `Gen` also have instances for `F`.

## Product types

Generic programming on product types typically consist of creating a value of the higher kinded
representation either through `tabulateK` or `to`, and ending with a call to a fold function
or `from`.

### Product examples: circe encoders and decoders

Here are some examples of deriving circe `Encoder`s and `Decoder`s using `HKDProductGeneric`.

```scala 3 sc:nocompile
trait PerspectiveProductDecoder[A] extends Decoder[A]

object PerspectiveProductDecoder:
  def derivedProductDecoder[A](
    using gen: HKDProductGeneric[A],
    decoders: gen.Gen[Decoder]
  ): PerspectiveProductDecoder[A] = new PerspectiveProductDecoder[A]:
    val names = gen.names

    override def apply(cursor: HCursor): Either[DecodingFailure, A] =
      // tabulateTraverseKEither is a fused version of tabulateK and traverseK specialized on Either
      gen.tabulateTraverseKEither {
        [X] => (idx: gen.Index[X]) =>
          val name = names.indexK(idx)
          val decoder = decoders.indexK(idx)
          cursor.get(name)(using decoder)
      }.map(gen.from)

  inline def derived[A](using gen: HKDGeneric[A]): PerspectiveProductDecoder[A] = inline gen match
    case gen: HKDProductGeneric.Aux[A, gen.Gen] =>
      val decoders = summonInline[gen.Gen[Decoder]]
      derivedProductDecoder(using gen, decoders)


trait PerspectiveProductEncoder[A] extends Encoder[A]

object PerspectiveProductEncoder:

  def derivedProductEncoder[A](
    using gen: HKDProductGeneric[A],
    encoders: gen.Gen[Encoder]
  ): PerspectiveProductEncoder[A] = new PerspectiveProductEncoder[A]:
    val names = gen.names

    override def apply(a: A): Json =
      val list: List[(String, Json)] =
        gen.tabulateFoldLeft(Nil: List[(String, Json)]) { acc =>
          [X] => (idx: gen.Index[X]) =>
            // productElementId allows indexing a value of type A without calling gen.to on it first
            val v = a.productElementId(idx)
            val name = names.indexK(idx)
            val encoder = encoders.indexK(idx)
            (name, encoder(v)) :: acc
        }

      Json.obj(list: _*)

  inline def derived[A](using gen: HKDGeneric[A]): PerspectiveProductEncoder[A] = inline gen match
    case gen: HKDProductGeneric.Aux[A, gen.Gen] =>
      val encoders = summonInline[gen.Gen[Encoder]]
      derivedProductEncoder(using gen, encoders)
```

## Sum types

`HKDSumGeneric` works in many ways like `HKDProductGeneric`, but with some differences. The higher
kinded type in this case has one value for each case of the sum type. `HKDSumGeneric` also provides
a new family of functions `indexOf` that takes a value of the sum type, and returns its index.

```scala 3 sc:nocompile
trait HKDSumGeneric[A] extends HKDGeneric[A]:
  type ElemTop <: A

  def indexOf[X <: ElemTop](x: X): Index[X]
```

The functions `from` and `to` functions also have different types. `def to(a: A): Gen[Option]`
and `def from(gen: Gen[Option]): Option[A]`. `to` fills all the slots of the higher kinded type
with `None` except for the slot that corresponds to the index of the value, which is filled with the
value. `from` returns `Some` if only one slot in the higher kinded type is `None`, and all the
others are `None`.

### Sum examples: circe encoders and decoder

To deal with enums, we need a function that derives instances for all the sum type cases
automatically. I have sadly not found any way to deal with it here. It can be seen here in the
functions `caseDecoders` and `caseDecoders`. These examples also show how names and indices can
interact.

```scala 3 sc:nocompile
trait PerspectiveProductDecoder[A] extends Decoder[A]

object PerspectiveProductDecoder:

  def derivedSumDecoder[A](
    using gen: HKDSumGeneric[A],
    decoders: gen.Gen[Decoder]
  ): PerspectiveProductDecoder[A] = new PerspectiveProductDecoder[A]:
    override def apply(cursor: HCursor): Either[DecodingFailure, A] =
      for
        typeNameStr <- cursor.get[String]("$type")
        typeName <- gen
          .stringToName(typeNameStr)
          .toRight(DecodingFailure(s"$typeNameStr is not a valid ${gen.typeName}", cursor.history))
        index = gen.nameToIndex(typeName)
        decoder = decoders.indexK(index)
        valueCursor = cursor.downField("$value")
        res <- decoder(cursor.downField("$value").success.getOrElse(cursor))
      yield res

  private inline def caseDecoders[T <: Tuple, R <: Tuple](builder: Helpers.TupleBuilder[R]): R =
    inline erasedValue[T] match
      case _: (h *: t) =>
        builder += summonFrom {
          case d: Decoder[`h`] => d
          case given HKDGeneric[`h`] => derived[h]
        }
        caseDecoders[t, R](builder)
      case _: EmptyTuple => builder.result

  inline def derived[A](using gen: HKDGeneric[A]): PerspectiveProductDecoder[A] = inline gen match
    case gen: HKDSumGeneric.Aux[A, gen.Gen] =>
      summonFrom {
        case decoders: gen.Gen[Decoder] => derivedSumDecoder(using gen, decoders)
        case _ =>
          val decoders = gen.tupleToGen(
            caseDecoders[gen.TupleRep, Helpers.TupleMap[gen.TupleRep, Decoder]](Helpers.TupleBuilder.mkFor)
          )
          derivedSumDecoder(using gen, decoders)
      }

trait PerspectiveSumEncoder[A] extends Encoder[A]

object PerspectiveSumEncoder:

  def derivedSumEncoder[A](
    using gen: HKDSumGeneric[A],
    encoders: gen.Gen[Encoder]
  ): PerspectiveSumEncoder[A] = new PerspectiveSumEncoder[A]:
    override def apply(a: A): Json =
      val typeName = (gen.indexToName(gen.indexOfA(a)): String).asJson

      val encodings =
        gen
          .to(a)
          .map2Const(encoders)([Z] => (optCase: Option[Z], encoder: Encoder[Z]) => optCase.map(x => encoder(x)))

      val json = encodings.indexK(gen.indexOfA(a)).get
      json.asObject match
        case Some(fields) => json.deepMerge(Json.obj("$type" -> typeName))
        case None => Json.obj("$type" -> typeName, "$value" -> json)

  private inline def caseEncoders[T <: Tuple, R <: Tuple](builder: Helpers.TupleBuilder[R]): R =
    inline erasedValue[T] match
      case _: (h *: t) =>
        builder += summonFrom {
          case d: Encoder[`h`] => d
          case given HKDGeneric[`h`] => derived[h]
        }
        caseEncoders[t, R](builder)
      case _: EmptyTuple => builder.result

  inline def derived[A](using gen: HKDGeneric[A]): PerspectiveSumEncoder[A] = inline gen match
    case gen: HKDSumGeneric.Aux[A, gen.Gen] =>
      summonFrom {
        case encoders: gen.Gen[Encoder] => derivedSumEncoder(using gen, encoders)
        case _ =>
          val encoders = gen.tupleToGen(
            caseEncoders[gen.TupleRep, Helpers.TupleMap[gen.TupleRep, Encoder]](Helpers.TupleBuilder.mkFor)
          )

          derivedSumEncoder(using gen, encoders)
      }
```