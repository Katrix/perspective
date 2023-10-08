package perspective

import scala.annotation.targetName
import scala.compiletime.*

import cats._

/**
  * A wrapper around an finite integer domain. Stores the size at the type
  * level.
  */
type Finite[N <: Int] = Finite.Finite[N]
object Finite:
  opaque type Finite[N <: Int] = Int

  extension [N <: Int](intValue: Finite[N])
    inline def value: Int = intValue

    @targetName("add") inline def +(other: Finite[N]): Finite[N] = (value + other.value) % constValue[N]

    @targetName("times") inline def *(other: Finite[N]): Finite[N] = (value * other.value) % constValue[N]

    @targetName("subtract") inline def -(other: Finite[N]): Finite[N] =
      Math.floorMod(value - other.value, constValue[N])

    def /(other: Finite[N]): (Finite[N], Finite[N]) =
      val quot: Finite[N] = value / other.value
      val rem: Finite[N]  = Math.floorMod(value, other.value)

      (quot, rem)
  end extension

  type NotZero[N <: Int] = N match
    case 0 => false
    case _ => true

  inline def apply[N <: Int](size: N, value: Int)(using NotZero[N] =:= true): Finite[N] =
    if value < size then value else Math.floorMod(value, size)

  given boundedEnumerable[N <: Int](using size: ValueOf[N]): kernel.BoundedEnumerable[Finite[N]] with {
    override def order: Order[Finite[N]] = (x: Finite[N], y: Finite[N]) => Integer.compare(x, y)

    override def maxBound: Finite[N] = size.value - 1

    override def partialPrevious(a: Finite[N]): Option[Finite[N]] = if a == 0 then None else Some(a - 1)

    override def partialNext(a: Finite[N]): Option[Finite[N]] = if a + 1 == size.value then None else Some(a + 1)

    override def minBound: Finite[N] = 0
  }
