package solver

sealed trait Expression

case class Constant(n: Double) extends Expression {
  override def toString = s"Const($n)"
}
case class Var(name: String) extends Expression {
  override def toString = s"Var($name)"
}

case class Sum(e1: Expression, e2: Expression) extends Expression {
  override def toString = { 
    val (l, r) = (e1.toString, e2.toString)

    s"Sum($l, $r)"
  }
}
case class Prod(e1: Expression, e2: Expression) extends Expression {
  override def toString = { 
    val (l, r) = (e1.toString, e2.toString)

    s"Prod($l, $r)"
  }
}

case class Power(b: Expression, e: Expression) extends Expression {
  override def toString = {
    val (bb, ee) = (b.toString, e.toString)
    s"Power($bb, $ee)"
  }
}


