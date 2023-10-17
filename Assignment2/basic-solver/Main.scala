package solver

object Solver extends App {
  // solves expString == 0 for the variable in varName with an initial guess
  // specified. We'll assume that the given expression has a root.

  def solve(expString: String, varName: String, guess: Double): Double = {
    val ex: Option[Expression] = Parser(expString)
    val f: Double => Double = x => Process.eval(ex.get, Map(varName -> x))
    val df: Expression = Process.differentiate(ex.get, varName)
    val dfx: Double => Double = x => Process.eval(df, Map(varName -> x))
    Newton.solve(f, dfx, guess).get
  }
//  val result = solve("x^2 - 4.0", "x", 1.0)
//  println(result == 2.0)
}
