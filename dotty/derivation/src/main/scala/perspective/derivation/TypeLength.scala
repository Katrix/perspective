package perspective.derivation

import scala.annotation.unused
import scala.compiletime.constValueTuple
import scala.deriving.Mirror
import scala.quoted.*

trait TypeLength[A] {
  type Length <: Int
  def length: Length
}
object TypeLength:
  case class TypeLengthImpl[A, Length0 <: Int](length: Length0) extends TypeLength[A] {
    type Length = Length0
  }

  type Aux[A, Length0 <: Int] = TypeLength[A] {
    type Length = Length0
  }

  inline given fromMirror[A](using m: Mirror.Of[A]): TypeLength.Aux[A, Tuple.Size[m.MirroredElemLabels]] =
    val v = constValueTuple[m.MirroredElemLabels].size
    new TypeLengthImpl[A, Tuple.Size[m.MirroredElemLabels]](v)

  given valueOf[A](using typeLength: TypeLength[A]): ValueOf[typeLength.Length] =
    ValueOf[typeLength.Length](typeLength.length)

  def tupleFromMirrorAndLength[A, T <: Tuple](
      using length: TypeLength[A],
      @unused m: Mirror.Of[A] { type MirroredElemTypes = T }
  ): TypeLength.Aux[T, length.Length] = length.asInstanceOf[TypeLength.Aux[T, length.Length]]
