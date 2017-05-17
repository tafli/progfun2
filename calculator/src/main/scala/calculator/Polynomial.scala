package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal(
    Math.pow(b(),2) - (4.0 * a() * c())
  )

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal(
    if(delta() < 0.0) Set()
    else {
      val sqrt = Math.sqrt(delta())
      Set(
        (-b() + sqrt) / (2*a()),
        (-b() - sqrt) / (2*a())
      )
    }
  )
}
