package perspective.examples

import perspective.derivation._
import io.circe._

object CodecDeriver {
  def deriver[A] = new CodecDeriver[A]

  class CodecDeriver[A] {
    def derive[Gen[_[_]]](
        implicit gen: HKDProductGeneric.Aux[A, Gen],
        encoders: Gen[Encoder],
        deceoders: Gen[Decoder]
    ): Codec[A] = Codec.from(DecoderDerive.productDecoder, EncoderDerive.productEncoder)
  }
}
