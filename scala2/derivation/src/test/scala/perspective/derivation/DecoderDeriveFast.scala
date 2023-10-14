package perspective.derivation

import io.circe._
import perspective._
import perspective.syntax.all._

object DecoderDeriveFast {

  implicit def productDecoder[A, Gen[_[_]]](
      implicit gen: HKDProductGeneric.Aux[A, Gen],
      decoders: Gen[Decoder]
  ): Decoder[A] = new Decoder[A] {
    private val names = gen.names

    override def apply(cursor: HCursor): Decoder.Result[A] = {
      import gen.implicits._

      gen
        .tabulateTraverseKEither[DecodingFailure, Id](
          Lambda[gen.Index ~>: Compose2[Decoder.Result, perspective.Id, *]] { idx =>
            val name    = names.indexKC(idx)
            val decoder = decoders.indexKC(idx)

            cursor.get(name)(decoder)
          }
        )
        .map(gen.from)
    }
  }

  def deriver[A] = new DecoderDeriver[A]

  class DecoderDeriver[A] {
    def deriveProduct[Gen[_[_]]](
        implicit gen: HKDProductGeneric.Aux[A, Gen],
        decoders: Gen[Decoder]
    ): Decoder[A] = productDecoder
  }
}
