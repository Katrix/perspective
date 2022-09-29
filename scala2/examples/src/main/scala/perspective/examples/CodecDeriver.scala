package perspective.examples

import io.circe._
import perspective.derivation._

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
