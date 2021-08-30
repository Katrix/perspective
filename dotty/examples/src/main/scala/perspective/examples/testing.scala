package perspective.examples

import scala.language.implicitConversions
import cats.Id
import cats.instances.either._
import io.circe._
import io.circe.syntax._
import perspective._
import perspective.derivation._

import scala.deriving._
import scala.compiletime.{erasedValue, summonFrom, summonInline}

trait PersepctiveDecoder[A] extends Decoder[A]
object PersepctiveDecoder:

  def derivedProductDecoder[A](
      using gen: HKDProductGeneric[A],
      decoders: gen.Gen[Decoder]
  ): Decoder[A] = new Decoder[A]:
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
  ): Decoder[A] = new Decoder[A]:
    override def apply(cursor: HCursor): Either[DecodingFailure, A] =
      import gen.given

      for
        typeNameStr <- cursor.get[String]("$type")
        typeName <- gen.stringToName(typeNameStr).toRight(DecodingFailure(s"$typeNameStr is not a valid ${gen.typeName}", cursor.history))
        index = gen.nameToIndex(typeName)
        decoder = decoders.indexK(index)
        valueCursor = cursor.downField("$value")
        res <- decoder(cursor.downField("$value").success.getOrElse(cursor))
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

trait PersepctiveEncoder[A] extends Encoder[A]
object PersepctiveEncoder:

  def derivedProductEncoder[A](
      using gen: HKDProductGeneric[A],
      encoders: gen.Gen[Encoder]
  ): Encoder[A] = new Encoder[A]:
    override def apply(a: A): Json =
      import gen.given

      val list: List[(String, Json)] =
        gen
          .to(a)
          .map2Const(encoders)([Z] => (caseObj: Z, encoder: Encoder[Z]) => encoder(caseObj))
          //TODO Type needed
          .map2Const[Const[gen.Names], (gen.Names, Json)](gen.names)([Z] => (json: Json, name: gen.Names) => (name, json))
          .toListK

      Json.obj(list: _*)

  def derivedSumEncoder[A](
      using gen: HKDSumGeneric[A],
      encoders: gen.Gen[Encoder]
  ): Encoder[A] = new Encoder[A]:
    override def apply(a: A): Json =
      import gen.given

      val typeName = (gen.indexToName(gen.indexOf(a)): String).asJson

      val encodings = 
        gen
          .to(a)
          .map2Const(encoders)([Z] => (optCase: Option[Z], encoder: Encoder[Z]) => optCase.map(x => encoder(x)))

      val json = encodings.indexK(gen.indexOf(a)).get
      json.asObject match
        case Some(fields) => json.deepMerge(Json.obj("$type" -> typeName))
        case None         => Json.obj("$type" -> typeName, "$value" -> json)

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

case class Foo(i: Int, s: String, foobar: Long) //derives PersepctiveEncoder//, PersepctiveDecoder
object Foo {
  given PersepctiveEncoder[Foo] = summon[PersepctiveEncoder[Foo]]
  given PersepctiveDecoder[Foo] = summon[PersepctiveDecoder[Foo]]

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

object Foo2 {

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
