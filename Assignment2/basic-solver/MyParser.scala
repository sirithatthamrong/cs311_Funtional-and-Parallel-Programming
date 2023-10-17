package solver

import scala.quoted.Expr

object MyParser extends App {

  sealed trait Token
  case class Const(x: Double) extends Token
  case object Times extends Token
  case object Plus extends Token
  case object Minus extends Token
  case object Expo extends Token
  case object LParen extends Token
  case object RParen extends Token
  case class Var(x: String) extends Token

  // your implementation of a parser that takes in an input String
  // and returns an option: None if the input is not a well-formed
  // expression. Otherwise, it evaluates to Some(e) where e is an
  // Expression which encodes the input string.
  def parse(input: String): Option[Expression] = {
    ???
  }


  def tokenize(input:String): Option[List[Token]] = {
    val const = """(\d+(\.\d+){0,1})""".r
    val vari = "([a-z])".r
    val (lparen, rparen, plus, minus, time, expo) =
      ("\\(".r, "\\)".r, "\\+".r, "\\-".r, "\\*".r, "\\^".r)

    val TOKEN_PATTERN = "(\\(|\\)|\\d+(\\.\\d+){0,1}|[\\+\\-/]|\\*|\\^|[a-z])".r
    case object BadToken extends Exception

    try {
      val tokens = TOKEN_PATTERN.findAllMatchIn(input)
        .toList
        .map {
          case const(number,_*) => Const(number.toDouble)
          case lparen(_*) => LParen
          case rparen(_*) => RParen
          case plus(_*) => Plus
          case minus(_*) => Minus
          case time(_*) => Times
          case expo(_*) => Expo
          case vari(name) => Var(name.trim)
          case _ => throw BadToken
        }

        Some(tokens)
      } catch { case BadToken => None }
  }
  def apply(input: String): Option[Expression] =
    parse(input)
}

