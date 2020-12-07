object C {
  inline def +=(inline tuple: (Int, Int)) = inline tuple match { 
    case (a, b) => add(a, b) 
  }
  def add(a: Int, b: Int) = C
  
  C += (1, 2)
}