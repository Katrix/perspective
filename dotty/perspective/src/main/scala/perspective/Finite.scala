package perspective

/**
  * A wrapper around an finite integer domain. Stores the size at the type level.
  */
case class Finite[N <: Int] private (value: Int):

  def +(other: Finite[N])(using n: ValueOf[N]): Finite[N] = new Finite((value + other.value) % n.value)
  def *(other: Finite[N])(using n: ValueOf[N]): Finite[N] = new Finite((value * other.value) % n.value)
  def -(other: Finite[N])(using n: ValueOf[N]): Finite[N] = new Finite(Math.floorMod(value - other.value, n.value))
  def /(other: Finite[N]): (Finite[N], Finite[N]) =
    val quot: Finite[N] = new Finite(value / other.value)
    val rem: Finite[N]  = new Finite(Math.floorMod(value, other.value))

    (quot, rem)

object Finite:
  type NotZero[N <: Int] = N match
    case 0 => false
    case _ => true
  
  def apply[N <: Int](size: N, value: Int)(using NotZero[N] =:= true): Finite[N] =
    new Finite[N](Math.floorMod(value, size))
