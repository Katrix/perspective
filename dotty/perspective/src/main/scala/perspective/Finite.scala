package perspective

/**
  * A wrapper around an finite integer domain. Stores the size at the type level.
  */
case class Finite[N <: Int with Singleton] private (value: Int):

  def +(other: Finite[N])(implicit n: ValueOf[N]): Finite[N] = new Finite((value + other.value) % n.value)
  def *(other: Finite[N])(implicit n: ValueOf[N]): Finite[N] = new Finite((value * other.value) % n.value)
  def -(other: Finite[N])(implicit n: ValueOf[N]): Finite[N] = new Finite(Math.floorMod(value - other.value, n.value))
  def /(other: Finite[N]): (Finite[N], Finite[N]) = {
    val quot: Finite[N] = new Finite(value / other.value)
    val rem: Finite[N]  = new Finite(Math.floorMod(value, other.value))

    quot -> rem
  }

object Finite:
  def apply[N <: Int with Singleton: NotZero](size: N, value: Int): Finite[N] =
    new Finite[N](Math.floorMod(value, size))

sealed trait NotZero[N <: Int with Singleton]
object NotZero:
  given notZeroN[N <: Int with Singleton] as NotZero[N] {}
  //Ambigous implicit from two definitions at 0
  given notZeroZero1 as NotZero[0] {}
  given notZeroZero2 as NotZero[0] {}
