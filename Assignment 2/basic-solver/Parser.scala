package solver

import scala.util.parsing.combinator._

/*
 * The parser object which produces a parsed expression for a given string.
 * As an example,
 *    val e = solver.Parser("x^2 + 3*x - 1")
 */

object Parser extends JavaTokenParsers {
  def expr: Parser[Expression] = term ~ (("+"|"-") ~ term).*  ^^ {
    case term ~ Nil => term
    case term ~ repTerms => repTerms.foldLeft(term) {
      case (termSoFar, "+" ~ nextTerm) => Sum(termSoFar, nextTerm)
      case (termSoFar, "-" ~ nextTerm) => Sum(termSoFar, Prod(Constant(-1), nextTerm))
      case _ => throw new Exception("shouldn't get here")
    }
  }
  def term: Parser[Expression] = expo ~ ("*" ~ expo).*  ^^ {
    case ex ~ Nil => ex
    case ex ~ repExs => repExs.foldLeft(ex) {
      case (exSoFar, "*" ~ nextEx) => Prod(exSoFar, nextEx)
      case _ => throw new Exception("shouldn't get here")
    }
  }

  def expo: Parser[Expression] = op ~ ("^" ~ op).?  ^^ {
    case op ~ None => op
    case base ~ Some("^" ~ expn) => Power(base, expn)
    case _ => throw new Exception("shouldn't get here")
  }

  def op: Parser[Expression] = ( const | variable | ("(" ~> expr <~ ")") )

  def variable: Parser[Expression] = ("""[a-zA-Z]""".r) ^^ {
    case varName => Var(varName)
  }
  def const: Parser[Expression] = floatingPointNumber ^^ {
    case numStr => Constant(numStr.toFloat)
  }

  def apply(input: String): Option[Expression] = 
    parseAll(expr, input) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
}
