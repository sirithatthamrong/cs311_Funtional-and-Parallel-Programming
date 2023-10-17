package solver

object Newton extends App {

  // your implementation of the Newton method that takes a function f: Double => Double
  // and its derivative df: Double => Double  (take note of the types),
  // and computes a root of f using the Newton's method with the given 
  // guess: Double starting value

  def solve(f: Double => Double, df: Double => Double, guess: Double = 1.0): Option[Double] = {
    def newtonStep(x: Double): Double = x - f(x) / df(x)
    def iterate(x: Double): Option[Double] = {
      val nextX = newtonStep(x)
      if (Math.abs(nextX - x) < 1e-10) Some(nextX)
      else iterate(nextX)
    }
    iterate(guess)
  }
//  val f: Double => Double = x => x*x - 4.0
//  val df: Double => Double = x => 2 * x
//  val result = Newton.solve(f, df)
//  println(result) // Some(2.0)
}
