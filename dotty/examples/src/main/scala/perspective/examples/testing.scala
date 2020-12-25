package perspective.examples

import scala.language.implicitConversions
import cats.Id
import cats.instances.either._
import perspective._
import perspective.derivation._

import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom, summonInline}

trait ACursor:
  def get[A: Decoder](field: String): Either[String, A]

trait Decoder[A]:
  def decode(cursor: ACursor): Either[String, A]

object Decoder:
  given Decoder[Json]              = ???
  given Decoder[Int]               = ???
  given Decoder[Long]              = ???
  given Decoder[Double]            = ???
  given Decoder[String]            = ???
  given Decoder[Boolean]           = ???
  given Decoder[Map[String, Json]] = ???

  given[A: Decoder]: Decoder[Option[A]] = ???
  given[A: Decoder]: Decoder[Seq[A]]       = ???

  def derivedProductDecoder[A](
      using gen: HKDProductGeneric[A],
      decoders: gen.Gen[Decoder]
  ): Decoder[A] = new Decoder[A]:
    override def decode(cursor: ACursor): Either[String, A] =
      import gen.given

      gen.names
        .map2K(decoders)(
          [Z] => (name: String, decoder: Decoder[Z]) => cursor.get(name)(using decoder)
        )
        .sequenceIdK
        .map(gen.from)

  def derivedSumDecoder[A](
      using gen: HKDSumGeneric[A],
      decoders: gen.Gen[Decoder]
  ): Decoder[A] = new Decoder[A]:
    override def decode(cursor: ACursor): Either[String, A] =
      import gen.given

      for
        typeName <- cursor.get[String]("$type")
        index <- gen.nameToIndexMap.get(typeName).toRight(s"$typeName is not a valid ${gen.typeName}")
        decoder: Decoder[_ <: A] = decoders.indexK(index) //TODO Type needed. Inffered to Any otherwise
        res <- decoder.decode(cursor.get[Json]("$value").fold(_ => cursor, _.cursor))
      yield res

  private inline def caseDecoders[XS <: Tuple]: Tuple.Map[XS, Decoder] = inline erasedValue[XS] match
    case _: EmptyTuple => EmptyTuple
    case _: (h *: t) =>
      val headDecoder: Decoder[h] = summonFrom {
        case d: Decoder[`h`] => d
        case gen: HKDGeneric[`h`] => derived[h]
      }
      headDecoder *: caseDecoders[t]

  inline given derived[A](using gen: HKDGeneric[A]): Decoder[A] = inline gen match
    case gen: HKDProductGeneric.Aux[A, gen.Gen] =>
      given gen.Gen[Decoder] = summonInline[gen.Gen[Decoder]]
      derivedProductDecoder(using gen)
    case gen: HKDSumGeneric.Aux[A, gen.Gen] =>
      summonFrom {
        case decoders: gen.Gen[Decoder] => derivedSumDecoder(using gen, decoders)
        case _ =>
          val decodersTuple = caseDecoders[gen.TupleRep]
          val decoders = gen.tupleToGen(decodersTuple)
          derivedSumDecoder(using gen, decoders)
      }

trait Json:
  def fields: Option[Map[String, Json]]
  def cursor: ACursor

trait Encoder[A]:
  def encode(a: A): Json

object Encoder:
  given Encoder[Int]               = ???
  given Encoder[String]            = ???
  given Encoder[Double]            = ???
  given Encoder[Long]              = ???
  given Encoder[Boolean]           = ???
  given Encoder[Map[String, Json]] = ???

  given[A: Encoder]: Encoder[Option[A]] = ???
  given[A: Encoder]: Encoder[Seq[A]]    = ???

  def derivedProductEncoder[A](
      using gen: HKDProductGeneric[A],
      encoders: gen.Gen[Encoder]
  ): Encoder[A] = new Encoder[A]:
    override def encode(a: A): Json =
      import gen.given

      val list: List[(String, Json)] =
        gen
          .to(a)
          .map2Const(encoders)([Z] => (caseObj: Z, encoder: Encoder[Z]) => encoder.encode(caseObj))
          //TODO Type needed
          .map2Const[Const[Json], Const[String], (String, Json), Nothing](gen.names)([Z] => (json: Json, name: String) => (name, json))
          .toListK

      implicitly[Encoder[Map[String, Json]]].encode(list.toMap)

  def derivedSumEncoder[A](
      using gen: HKDSumGeneric[A],
      encoders: gen.Gen[Encoder]
  ): Encoder[A] = new Encoder[A]:
    override def encode(a: A): Json =
      import gen.given

      val typeName = implicitly[Encoder[String]].encode(gen.indexToNameMap(gen.indexOf(a)))

      val encodings = 
        gen
          .to(a)
          .map2Const(encoders)([Z] => (optCase: Option[Z], encoder: Encoder[Z]) => optCase.map(x => encoder.encode(x)))

      val json = encodings.indexK(gen.indexOf(a)).get
      json.fields match
        case Some(fields) => implicitly[Encoder[Map[String, Json]]].encode(fields.updated("$type", typeName))
        case None         => implicitly[Encoder[Map[String, Json]]].encode(Map("$type" -> typeName, "$value" -> json))

  private inline def caseEncoders[XS <: Tuple]: Tuple.Map[XS, Encoder] = inline erasedValue[XS] match
    case _: EmptyTuple => EmptyTuple
    case _: (h *: t) => 
      val headEncoder: Encoder[h] = summonFrom {
        case e: Encoder[`h`] => e
        case gen: HKDGeneric[`h`] => derived[h]
      }
      headEncoder *: caseEncoders[t]

  inline given derived[A](using gen: HKDGeneric[A]): Encoder[A] = inline gen match
    case gen: HKDProductGeneric.Aux[A, gen.Gen] => 
      given gen.Gen[Encoder] = summonInline[gen.Gen[Encoder]]
      derivedProductEncoder(using gen)
    case gen: HKDSumGeneric.Aux[A, gen.Gen] =>
      summonFrom {
        case encoders: gen.Gen[Encoder] => derivedSumEncoder(using gen, encoders)
        case _ =>
          val encodersTuple = caseEncoders[gen.TupleRep]
          val encoders = gen.tupleToGen(encodersTuple)
          derivedSumEncoder(using gen, encoders)
      }

trait Codec[A] extends Encoder[A] with Decoder[A]
object Codec:

  given derived[A](using encoder: Encoder[A], decoder: Decoder[A]): Codec[A]:
    override def decode(cursor: ACursor): Either[String, A] = decoder.decode(cursor)

    override def encode(a: A): Json = encoder.encode(a)
  

case class Foo(i: Int, s: String, foobar: Long) derives Encoder, Decoder

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
) derives Encoder, Decoder

enum Baz1 derives Encoder, Decoder {
  case Baz11(i: Int)
  case Baz12(s: String, l: Long)
}

sealed trait Baz2 derives Encoder, Decoder
object Baz2 {
  case class Baz21(i: Int) extends Baz2 derives Encoder, Decoder
  case class Baz22(s: String, l: Long) extends Baz2 derives Encoder, Decoder
}

//@main def nameTest = println(HKDProductGeneric.derived[Foo].names)

object Foo {

  //summon[Decoder[Foo]]
  
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

  //summon[Encoder[(String, Int)]]

  //summon[Decoder[Bar]]
  //HKDProductGeneric[Bar1]
  //HKDProductGeneric[Bar1]
  //HKDProductGeneric[Bar1]
  //HKDProductGeneric[Bar1]
  //HKDProductGeneric[Bar1]
}
