package solver

object Process extends App {
  // gives a "pretty-print" string form of the expression
  def stringify(e: Expression): String = e match {
    case Constant(c) => c.toString
    case Var(name) => name
    case Sum(l, r) => stringify(l) + " + " + stringify(r)
    case Prod(l@Sum(_, _), r@Sum(_, _)) => "(" + stringify(l) + ") * (" + stringify(r) + ")"
    case Prod(l@Sum(_, _), r) => "(" + stringify(l) + ") * " + stringify(r)
    case Prod(l, r@Sum(_, _)) => stringify(l) + " * (" + stringify(r) + ")"
    case Prod(l, r) => stringify(l) + " * " + stringify(r)
    case Power(b, e) => stringify(b) + "^" + stringify(e)
  }

  // evaluates a given expression e: Expression using
  // the variable settings in varAssn: Map[String, Double],
  // returning the evaluation result as a Double.

  // Example: eval(e, Map("x" -> 4.0)) evaluates the expression 
  // with the variable "x" set to 4.0.
  def eval(e: Expression, varAssn: Map[String, Double]): Double = e match {
    case Constant(c) => c
    case Var(name) => varAssn(name)
    case Sum(l, r) => eval(l, varAssn) + eval(r, varAssn)
    case Prod(l, r) => eval(l, varAssn) * eval(r, varAssn)
    case Power(b, e) => math.pow(eval(b, varAssn), eval(e, varAssn))
  }
//  val e1 = solver.Parser("x^2 + 3*x - 1")
//  println(eval(e1.get, Map("x" -> 4.0)) == 27.0)
//  val e2 = solver.Parser("5^2 + y + x")
//  println(eval(e2.get, Map("x" -> 2.0, "y" -> -2.0)) == 25.0)


  // symbolically differentiates an expression e: Expression with 
  // respect to the variable varName: String
  def differentiate(e: Expression, varName: String): Expression = e match {
    case Constant(_) => Constant(0.0) // diff const = 0
    case Var(name)  =>
      if (name == varName) Constant(1.0) // d/dx(x) = 1
      else Constant(0.0)
    case Sum(l, r) =>
      Sum(simplify(differentiate(l, varName)), simplify(differentiate(r, varName)))
    case Prod(l, r) =>
      Sum(simplify(Prod(differentiate(l, varName), r)), simplify(Prod(l, differentiate(r, varName))))
    case Power(Constant(c), e) =>
      Prod(simplify(Power(Constant(c), e)), Constant(Math.log(c)))
    case Power(b, e) =>
      if (!stringify(b).contains(varName)) Constant(0.0)
      else Prod(e, simplify(Power(b, simplify(Sum(e, Constant(-1.0))))))
  }
//  val e1 = solver.Parser("x^2 + 3*x - 1")
//  println(stringify(differentiate(e1.get, "x"))) // 2.0 * x + 3.0 + 0.0
//  val e2 = solver.Parser("2^(x^2)")
//  println(stringify(differentiate(e2.get, "x"))) // 2.0^x^2.0 * 0.6931471805599453
//  val e3 = solver.Parser("x^3 + 2")
//  println(stringify(differentiate(e3.get, "y"))) // 0.0
//  val e4 = solver.Parser("x*y + 2")
//  println(stringify(differentiate(e4.get, "y"))) // x + 0.0
//  val e5 = solver.Parser("x*y + x^2 - 3")
//  println(stringify(differentiate(e5.get, "x"))) // y + 2*x

  // forms a new expression that simplifies the given expression e: Expression
  // the goal of this function is produce an expression that is easier to
  // evaluate and/or differentiate.  If there's a canonical form you'd like to
  // follow, use this function to put an expression in this form.
  // you may leave this function blank if can't find any use. 
  def simplify(e: Expression): Expression = e match {
    case Sum(Constant(a), Constant(b)) => Constant(a + b)
    case Sum(Constant(0.0), b) => b
    case Sum(a, Constant(0.0)) => a
    case Prod(Constant(a), Constant(b)) => Constant(a * b)
    case Prod(Constant(0.0), b) => Constant(0.0)
    case Prod(a, Constant(0.0)) => Constant(0.0)
    case Prod(Constant(1.0), b) => b
    case Prod(a, Constant(1.0)) => a
    case Power(Constant(1.0), b) => b
    case Power(a, Constant(1.0)) => a
    case _ => e
  }
}
