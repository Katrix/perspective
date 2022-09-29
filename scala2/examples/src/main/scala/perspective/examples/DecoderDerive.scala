package perspective.examples

import io.circe._
import perspective._
import perspective.derivation._
import perspective.syntax.all._

object DecoderDerive {

  implicit def productDecoder[A, Gen[_[_]]](
      implicit gen: HKDProductGeneric.Aux[A, Gen],
      decoders: Gen[Decoder]
  ): Decoder[A] = new Decoder[A] {
    override def apply(cursor: HCursor): Decoder.Result[A] = {
      import gen.implicits._

      gen.names
        .map2KC(decoders)(
          Lambda[Tuple2K[Const[String, *], Decoder, *] ~>: Decoder.Result](t => cursor.get(t._1)(t._2))
        )
        .sequenceIdKC
        .map(gen.from)
    }
  }

  def deriver[A] = new DecoderDeriver[A]

  class DecoderDeriver[A] {
    def deriveProduct[Gen[_[_]]](
        implicit gen: HKDProductGeneric.Aux[A, Gen],
        decoders: Gen[Decoder]
    ): Decoder[A] = productDecoder

    def deriveSum[Gen[_[_]]](
        implicit gen: HKDSumGeneric.Aux[A, Gen],
        decoders: Gen[Decoder]
    ): Decoder[A] = sumDecoder
  }

  implicit def sumDecoder[A, Gen[_[_]]](
      implicit gen: HKDSumGeneric.Aux[A, Gen],
      decoders: Gen[Decoder]
  ): Decoder[A] = new Decoder[A] {
    override def apply(cursor: HCursor): Decoder.Result[A] = {
      import gen.implicits._

      for {
        typeName <- cursor.get[String]("$type")
        index <-
          gen.nameToIndexMap
            .get(typeName)
            .toRight(DecodingFailure(s"$typeName is not a valid ${gen.typeName}", cursor.downField("$type").history))
        decoder = decoders.indexKC(index)
        res <- decoder.apply(cursor.get[Json]("$value").fold(_ => cursor, _.hcursor))
      } yield res
    }
  }
}
