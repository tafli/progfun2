package calculator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, ShouldMatchers}

@RunWith(classOf[JUnitRunner])
class PolynomialSuite extends FunSuite with ShouldMatchers {
  test("Delta of all a=0 / b=0 /c=0 should be 0") {
    val result = Polynomial.computeDelta(Signal(0), Signal(0), Signal(0))
    assert(result() === 0)
  }

  test("Delta of all a=1 / b=1 /c=1 should be -3") {
    val result = Polynomial.computeDelta(Signal(1), Signal(1), Signal(1))
    assert(result() === -3)
  }

  test("Delta of all a=4 / b=3 /c=2 should be -23") {
    val result = Polynomial.computeDelta(Signal(4), Signal(3), Signal(2))
    assert(result() === -23)
  }
}
