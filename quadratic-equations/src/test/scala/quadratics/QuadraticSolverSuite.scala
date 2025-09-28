package quadratics

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

class QuadraticSolverSuite extends ScalaCheckSuite {
  test("quadratic solver unit tests") {
    val a:Double = 1
    val b:Double = -3
    val c:Double = 2
    val expected:List[Double] = List(1, 2)
    val obtained:List[Double] = solve(a, b, c)
    assertEquals(obtained, expected)
  }
  
  property("solve returns single root for perfect square") {
    forAll { (x: Double) =>
        val roots = solve(1, -2*x, x*x)
        roots.length == 1
    }
  }
}
