package perspective

/**
  * A wrapper around an integer finite domain. Stores the size at the type level.
  */
case class Finite[N <: Int with Singleton] private (size: N, value: Int) {

  def +(other: Finite[N]): Finite[N] = Finite(size, value + other.value)
  def *(other: Finite[N]): Finite[N] = Finite(size, value * other.value)
  def -(other: Finite[N]): Finite[N] = Finite(size, value - other.value)
  def /(other: Finite[N]): (Finite[N], Finite[N]) = {
    val quot: Finite[N] = Finite(size, value / other.value)
    val rem: Finite[N] = Finite(size, Math.floorMod(value, other.value))

    quot -> rem
  }
}
object Finite {
  def apply[N <: Int with Singleton](size: N, value: Int): Finite[N] = new Finite[N](size, Math.floorMod(value, size))
}
