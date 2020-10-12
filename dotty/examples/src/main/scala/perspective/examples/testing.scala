package perspective.examples

import cats.Id
import cats.instances.either._
import perspective._
import perspective.derivation._
import scala.deriving._
import scala.compiletime.summonInline

trait ACursor:
  def get[A: Decoder](field: String): Either[String, A]

trait Decoder[A]:
  def decode(cursor: ACursor): Either[String, A]

object Decoder:
  given Decoder[Int]               = ???
  given Decoder[Long]              = ???
  given Decoder[Double]            = ???
  given Decoder[String]            = ???
  given Decoder[Boolean]           = ???
  given Decoder[Map[String, Json]] = ???

  given[A: Decoder] as Decoder[Option[A]] = ???
  given[A: Decoder] as Decoder[Seq[A]]       = ???

  given derivedProductDecoder[A](
      using gen: HKDProductGeneric[A],
      decoders: gen.Gen[Decoder]
  ) as Decoder[A]:
    override def decode(cursor: ACursor): Either[String, A] =
      import gen.{given _}

      gen.names
        .map2K(decoders)(
          [Z] => (t: (String, Decoder[Z])) => cursor.get(t._1)(t._2)
        )
        .sequenceIdK
        .map(gen.from)

  given derivedSumDecoder[A](
      using gen: HKDSumGeneric[A],
      decoders: gen.Gen[Decoder]
  ) as Decoder[A]:
    override def decode(cursor: ACursor): Either[String, A] =
      import gen.{given _}

      for
        typeName <- cursor.get[String]("$type")
        index <- gen.indexNameMap.get(typeName).toRight(s"$typeName is not a valid ${gen.typeName}")
        decoder: Decoder[_ <: A] = decoders.indexK(index) //TODO Type needed. Inffered to Any otherwise
        res <- decoder.decode(cursor)
      yield res

  inline given derived[A](using m: Mirror.Of[A]) as Decoder[A] = inline m match
    case _: Mirror.ProductOf[A] => 
      val hkd = summonInline[HKDProductGeneric[A]]
      val decoders = summonInline[hkd.Gen[Decoder]]
      derivedProductDecoder(using hkd, decoders)
    case _: Mirror.SumOf[A] => 
      val hkd = summonInline[HKDSumGeneric[A]]
      val decoders = summonInline[hkd.Gen[Decoder]]
      derivedSumDecoder(using hkd, decoders)

trait Json

trait Encoder[A]:
  def encode(a: A): Json

object Encoder:
  given Encoder[Int]               = ???
  given Encoder[String]            = ???
  given Encoder[Double]            = ???
  given Encoder[Long]              = ???
  given Encoder[Boolean]           = ???
  given Encoder[Map[String, Json]] = ???

  given[A: Encoder] as Encoder[Option[A]] = ???
  given[A: Encoder] as Encoder[Seq[A]]    = ???

  given derivedProductEncoder[A](
      using gen: HKDProductGeneric[A],
      encoders: gen.Gen[Encoder]
  ) as Encoder[A]:
    override def encode(a: A): Json = 
      import gen.{given _}

      val list: List[(String, Json)] =
        gen
          .to(a)
          //TODO Type needed
          .map2K(encoders)([Z] => (t: (Z, Encoder[Z])) => t._2.encode(t._1): Const[Json][Z])
          //TODO Type needed
          .map2K[Const[Json], Const[String], Const[(String, Json)], Nothing](gen.names)([Z] => (t: (Json, String)) => t.swap)
          .toListK

      implicitly[Encoder[Map[String, Json]]].encode(list.toMap)

  given derivedSumEncoder[A](
      using gen: HKDSumGeneric[A],
      encoders: gen.Gen[Encoder]
  ) as Encoder[A]:
    override def encode(a: A): Json = 
      import gen.{given _}

      val encodings = 
        gen
          .to(a)
          .map2K(encoders)([Z] => (t: (Option[Z], Encoder[Z])) => t._1.map(x => t._2.encode(x)): Const[Option[Json]][Z])

      encodings.indexK(gen.indexOf(a)).get

  given derived[A](using gen: HKDGeneric[A], encoders: gen.Gen[Encoder]) as Encoder[A] = gen match
    case gen: HKDProductGeneric.Aux[A, gen.Gen] => derivedProductEncoder(using gen, encoders)
    case gen: HKDSumGeneric.Aux[A, gen.Gen] => derivedSumEncoder(using gen, encoders)

trait Codec[A] extends Encoder[A] with Decoder[A]
object Codec:

  given derived[A](using encoder: Encoder[A], decoder: Decoder[A]) as Codec[A]:
    override def decode(cursor: ACursor): Either[String, A] = decoder.decode(cursor)

    override def encode(a: A): Json = encoder.encode(a)
  

case class Foo(i: Int, s: String, foobar: Long) //derives Encoder

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
) //derives Encoder

//@main def nameTest = println(HKDProductGeneric.derived[Foo].names)

object Foo {

  //summon[Decoder[Foo]]
  HKDProductGeneric.make[Foo]
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
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
  //HKDProductGeneric[Bar]
}
