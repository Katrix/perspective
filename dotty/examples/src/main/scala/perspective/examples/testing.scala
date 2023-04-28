package perspective.examples

import scala.language.implicitConversions

import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving._

import cats.Id
import cats.instances.either._
import io.circe._
import io.circe.syntax._
import perspective._
import perspective.derivation._

trait PerspectiveDecoder[A] extends Decoder[A]
object PerspectiveDecoder:

  def derivedProductDecoder[A](
      using gen: HKDProductGeneric[A],
      decoders: gen.Gen[Decoder]
  ): PerspectiveDecoder[A] = new PerspectiveDecoder[A]:
    override def apply(cursor: HCursor): Either[DecodingFailure, A] =
      import gen.given

      gen.names
        .map2K(decoders)(
          [Z] => (name: gen.Names, decoder: Decoder[Z]) => cursor.get(name)(using decoder)
        )
        .sequenceIdK
        .map(gen.from)

  def derivedSumDecoder[A](
      using gen: HKDSumGeneric[A],
      decoders: gen.Gen[Decoder]
  ): PerspectiveDecoder[A] = new PerspectiveDecoder[A]:
    override def apply(cursor: HCursor): Either[DecodingFailure, A] =
      import gen.given

      for
        typeNameStr <- cursor.get[String]("$type")
        typeName <- gen
          .stringToName(typeNameStr)
          .toRight(DecodingFailure(s"$typeNameStr is not a valid ${gen.typeName}", cursor.history))
        index       = gen.nameToIndex(typeName)
        decoder     = decoders.indexK(index)
        valueCursor = cursor.downField("$value")
        res <- decoder(cursor.downField("$value").success.getOrElse(cursor))
      yield res

  private inline def caseDecoders[T <: Tuple, R <: Tuple](builder: Helpers.TupleBuilder[R]): R =
    inline erasedValue[T] match
      case _: (h *: t) =>
        builder += summonFrom {
          case d: Decoder[`h`]       => d
          case given HKDGeneric[`h`] => derived[h]
        }
        caseDecoders[t, R](builder)
      case _: EmptyTuple => builder.result

  inline given derived[A](using gen: HKDGeneric[A]): PerspectiveDecoder[A] = inline gen match
    case gen: HKDProductGeneric.Aux[A, gen.Gen] =>
      val decoders = summonInline[gen.Gen[Decoder]]
      derivedProductDecoder(using gen, decoders)
    case gen: HKDSumGeneric.Aux[A, gen.Gen] =>
      summonFrom {
        case decoders: gen.Gen[Decoder] => derivedSumDecoder(using gen, decoders)
        case _ =>
          val decoders = gen.tupleToGen(
            caseDecoders[gen.TupleRep, Helpers.TupleMap[gen.TupleRep, Decoder]](Helpers.TupleBuilder.mkFor)
          )
          derivedSumDecoder(using gen, decoders)
      }

trait PerspectiveEncoder[A] extends Encoder[A]
object PerspectiveEncoder:

  def derivedProductEncoder[A](
      using gen: HKDProductGeneric[A],
      encoders: gen.Gen[Encoder]
  ): PerspectiveEncoder[A] = new PerspectiveEncoder[A]:
    override def apply(a: A): Json =
      import gen.given

      val list: List[(String, Json)] =
        gen
          .to(a)
          .map2Const(encoders)([Z] => (caseObj: Z, encoder: Encoder[Z]) => encoder(caseObj))
          // TODO Type needed
          .map2Const[Const[gen.Names], (gen.Names, Json)](gen.names)(
            [Z] => (json: Json, name: gen.Names) => (name, json)
          )
          .toListK

      Json.obj(list: _*)

  def derivedSumEncoder[A](
      using gen: HKDSumGeneric[A],
      encoders: gen.Gen[Encoder]
  ): PerspectiveEncoder[A] = new PerspectiveEncoder[A]:
    override def apply(a: A): Json =
      import gen.given

      val typeName = (gen.indexToName(gen.indexOfA(a)): String).asJson

      val encodings =
        gen
          .to(a)
          .map2Const(encoders)([Z] => (optCase: Option[Z], encoder: Encoder[Z]) => optCase.map(x => encoder(x)))

      val json = encodings.indexK(gen.indexOfA(a)).get
      json.asObject match
        case Some(fields) => json.deepMerge(Json.obj("$type" -> typeName))
        case None         => Json.obj("$type" -> typeName, "$value" -> json)

  private inline def caseEncoders[T <: Tuple, R <: Tuple](builder: Helpers.TupleBuilder[R]): R =
    inline erasedValue[T] match
      case _: (h *: t) =>
        builder += summonFrom {
          case d: Encoder[`h`]       => d
          case given HKDGeneric[`h`] => derived[h]
        }
        caseEncoders[t, R](builder)
      case _: EmptyTuple => builder.result

  inline given derived[A](using gen: HKDGeneric[A]): PerspectiveEncoder[A] = inline gen match
    case gen: HKDProductGeneric.Aux[A, gen.Gen] =>
      val encoders = summonInline[gen.Gen[Encoder]]
      derivedProductEncoder(using gen, encoders)
    case gen: HKDSumGeneric.Aux[A, gen.Gen] =>
      summonFrom {
        case encoders: gen.Gen[Encoder] => derivedSumEncoder(using gen, encoders)
        case _ =>
          val encoders = gen.tupleToGen(
            caseEncoders[gen.TupleRep, Helpers.TupleMap[gen.TupleRep, Encoder]](Helpers.TupleBuilder.mkFor)
          )
          derivedSumEncoder(using gen, encoders)
      }

case class Foo(i: Int, s: String, foobar: Long) //derives PersepctiveEncoder//, PersepctiveDecoder
object Foo {
  given PerspectiveEncoder[Foo] = summon[PerspectiveEncoder[Foo]]
  given PerspectiveDecoder[Foo] = summon[PerspectiveDecoder[Foo]]

}

extension [A](a: A)(using gen: HKDProductGeneric[A]) {
  def foo[X <: gen.Names](x: X): gen.FieldOf[X] =
    import gen.given
    gen.to(a).indexK(gen.nameToIndex(x))
}

case class Cat(name: String)
val res = Cat("Garfield").foo("name")

/*

case class Bar1(
    i1_1: Int,
    i1_2: Int,
    i1_3: Int,
    i1_4: Int,
    i1_5: Int,
    i1_6: Int,
    i1_7: Int,
    i1_8: Int,
    i1_9: Int,
    i1_10: Int,
    i1_11: Int,
    i1_12: Int,
    i1_13: Int,
    i1_14: Int,
    i1_15: Int,
    i1_16: Int,
    i1_17: Int,
    i1_18: Int,
    i1_19: Int,
    i1_20: Int,
    i1_21: Int,
    i1_22: Int
) derives PersepctiveEncoder, PersepctiveDecoder

enum Baz1 derives PersepctiveEncoder, PersepctiveDecoder {
  case Baz11(i: Int)
  case Baz12(s: String, l: Long)
}

sealed trait Baz2 derives PersepctiveEncoder, PersepctiveDecoder
object Baz2 {
  case class Baz21(i: Int) extends Baz2 derives PersepctiveEncoder, PersepctiveDecoder
  case class Baz22(s: String, l: Long) extends Baz2 derives PersepctiveEncoder, PersepctiveDecoder
}
 */

//@main def nameTest = println(HKDProductGeneric.derived[Foo].names)

@main def loopTest =
  case class A(value: String) derives PerspectiveEncoder
  case class B(as: List[A]) derives PerspectiveEncoder
  println(A("foo").asJson)
  println(B(List(A("foo"), A("bar"), A("baz"))).asJson)

object Foo2 {

  // summon[Decoder[Foo]]

  /*
  summon[Encoder[Foo]](
    using Encoder.derived(
      using HKDProductGeneric.derived[Foo],
      summon[Product2K[Int, String, Encoder]](using Product2K.findInstances)
      summon[Product2K[Int, String, Encoder]](using Product2K.findInstances)
      summon[Product2K[Int, String, Encoder]](using Product2K.findInstances)
      summon[Product2K[Int, String, Encoder]](using Product2K.findInstances)
      summon[Product2K[Int, String, Encoder]](using Product2K.findInstances)
    )
  )
   */

  // summon[Encoder[(String, Int)]]

  // summon[Decoder[Bar]]
  // HKDProductGeneric[Bar1]
  // HKDProductGeneric[Bar1]
  // HKDProductGeneric[Bar1]
  // HKDProductGeneric[Bar1]
  // HKDProductGeneric[Bar1]
}

trait PerspectiveInlineDecoder[A] extends Decoder[A]
object PerspectiveInlineDecoder:

  inline def derivedProductDecoder[A](using gen: InlineHKDProductGeneric[A]): PerspectiveInlineDecoder[A] =
    new PerspectiveInlineDecoder[A]:
      private val names    = gen.names
      private val decoders = gen.summonInstances[Decoder]

      override def apply(cursor: HCursor): Either[DecodingFailure, A] =
        gen
          .tabulateTraverseIdK { i =>
            val name    = names.indexK(i)
            val decoder = decoders.indexK(i)

            cursor.get(name)(using decoder)
          }
          .map(gen.from(_))

  inline def derivedSumDecoder[A](using gen: InlineHKDSumGeneric[A]): PerspectiveInlineDecoder[A] =
    new PerspectiveInlineDecoder[A]:
      import gen.given

      private val names = gen.names
      private val decoders = summonFrom {
        case decoders: gen.Gen[Decoder] => decoders
        case _ =>
          gen.tupleToGen(
            caseDecoders[gen.TupleRep, Helpers.TupleMap[gen.TupleRep, Decoder]](Helpers.TupleBuilder.mkFor)
          )
      }

      override def apply(cursor: HCursor): Either[DecodingFailure, A] =
        cursor
          .get[String]("$type")
          .flatMap { typeNameStr =>
            gen
              .stringToName(typeNameStr)
              .toRight(DecodingFailure(s"$typeNameStr is not a valid ${gen.typeName}", cursor.history))
          }
          .flatMap { typeName =>
            val index   = gen.nameToIndex(typeName)
            val decoder = decoders.indexK(index)
            decoder(cursor.downField("$value").success.getOrElse(cursor))
          }

  private inline def caseDecoders[T <: Tuple, R <: Tuple](builder: Helpers.TupleBuilder[R]): R =
    inline erasedValue[T] match
      case _: (h *: t) =>
        builder += summonFrom {
          case d: Decoder[`h`]             => d
          case given InlineHKDGeneric[`h`] => derived[h]
        }
        caseDecoders[t, R](builder)
      case _: EmptyTuple => builder.result

  inline given derived[A](using gen: InlineHKDGeneric[A]): PerspectiveInlineDecoder[A] = inline gen match
    case gen: InlineHKDProductGeneric.Aux[A, gen.Gen] => derivedProductDecoder(using gen)
    case gen: InlineHKDSumGeneric.Aux[A, gen.Gen]     => derivedSumDecoder(using gen)

trait PerspectiveInlineEncoder[A] extends Encoder[A]
object PerspectiveInlineEncoder:

  inline def derivedProductEncoder[A](using gen: InlineHKDProductGeneric[A]): PerspectiveInlineEncoder[A] =
    new PerspectiveInlineEncoder[A]:
      private val names    = gen.names
      private val encoders = gen.summonInstances[Encoder]

      override def apply(a: A): Json =
        val obj = gen.to(a)

        val list = gen.tabulateFoldLeft(Nil: List[(String, Json)])((acc, idx) =>
          val field   = obj.indexK(idx)
          val name    = names.indexK(idx)
          val encoder = encoders.indexK(idx)

          (name, encoder(field)) :: acc
        )

        Json.obj(list: _*)

  inline def derivedSumEncoder[A](using gen: InlineHKDSumGeneric[A]): PerspectiveInlineEncoder[A] =
    import gen.given

    val encoders = summonFrom {
      case encoders: gen.Gen[Encoder] => encoders
      case _                          => gen.tupleToGen(caseEncoders[gen.TupleRep, Helpers.TupleMap[gen.TupleRep, Encoder]](Helpers.TupleBuilder.mkFor))
    }
    val names = gen.names

    new PerspectiveInlineEncoder[A]:
      override def apply(a: A): Json =
        val casted       = gen.indexOfACasting(a)
        val idx          = casted.index
        val obj          = casted.value
        val typeName     = names.indexK(idx)
        val typeNameJson = Json.fromString(typeName)
        val encoder      = encoders.indexK(idx)

        val json = encoder(obj)
        json.asObject match
          case Some(_) => json.deepMerge(Json.obj("$type" -> typeNameJson))
          case None    => Json.obj("$type" -> typeNameJson, "$value" -> json)

  private inline def caseEncoders[T <: Tuple, R <: Tuple](builder: Helpers.TupleBuilder[R]): R =
    inline erasedValue[T] match
      case _: (h *: t) =>
        builder += summonFrom {
          case d: Encoder[`h`] => d
          case given InlineHKDGeneric[`h`] => derived[h]
        }
        caseEncoders[t, R](builder)
      case _: EmptyTuple => builder.result

  inline given derived[A](using gen: InlineHKDGeneric[A]): PerspectiveInlineEncoder[A] = inline gen match
    case gen: InlineHKDProductGeneric.Aux[A, gen.Gen] => derivedProductEncoder(using gen)
    case gen: InlineHKDSumGeneric.Aux[A, gen.Gen]     => derivedSumEncoder(using gen)

object PerformanceTesting {
  def foo: (String, Int, Double, String, Boolean) =
    scala.compiletime.constValueTuple[("foo", 5, 3.14, "bar", false)]

  def bar: (String, Int, Double, String, Boolean) =
    perspective.derivation.Helpers.constValueTupleOptimized[("foo", 5, 3.14, "bar", false)]
}

sealed trait BarInline {
  given encoder: PerspectiveInlineEncoder[BarInline] =
    PerspectiveInlineEncoder.derivedSumEncoder[BarInline]

  given decoder: PerspectiveInlineDecoder[BarInline] = PerspectiveInlineDecoder.derivedSumDecoder[BarInline]
}
case class FooInline(i: Int, s: String, foobar: Long)
    extends BarInline //derives PersepctiveEncoder//, PersepctiveDecoder
object FooInline {

  given encoder: PerspectiveInlineEncoder[FooInline] =
    PerspectiveInlineEncoder.derivedProductEncoder[FooInline]

  given decoder: PerspectiveInlineDecoder[FooInline] =
    PerspectiveInlineDecoder.derivedProductDecoder[FooInline]
}
