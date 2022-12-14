package perspective.derivation

class ArrayProduct private (private val array: Array[Object]) extends Product {
  override def productArity: Int = array.length

  override def productElement(n: Int): Any = array(n)

  def canEqual(other: Any): Boolean = other.isInstanceOf[ArrayProduct]

  override def equals(other: Any): Boolean = other match
    case that: ArrayProduct =>
      that.canEqual(this) && java.util.Arrays.equals(array, that.array)
    case _ => false

  override def hashCode(): Int = {
    val state = array
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString = s"ArrayProduct(${array.mkString("Array(", ",", ")")})"
}
object ArrayProduct {
  def apply(array: IArray[Object]): ArrayProduct = new ArrayProduct(array.asInstanceOf[Array[Object]])

  def ofArrayUnsafe(array: Array[Object]): ArrayProduct = new ArrayProduct(array)
}
