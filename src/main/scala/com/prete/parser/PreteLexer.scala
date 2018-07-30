package com.prete.parser

import scala.util.parsing.combinator.RegexParsers


sealed trait PreteCompilationError
case class PreteLexerError(location: Location, msg: String) extends PreteCompilationError
case class PreteParserError(location: Location, msg: String) extends PreteCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}


sealed trait PreteToken
object Tokens {

  case class Symbol(name: java.lang.String) extends PreteToken

  case class Integer(value: java.lang.Integer) extends PreteToken

  case class Float(value: java.lang.Float) extends PreteToken

  case class String(value: java.lang.String) extends PreteToken
  case object Colon extends PreteToken
  case object OpenBracket extends PreteToken
  case object CloseBracket extends PreteToken

  case class IndentCount(length: Int) extends PreteToken
  case object Dedent extends PreteToken
  case object Indent extends PreteToken
  case object Arrow extends PreteToken


  case object DefObject extends PreteToken
  case object DefRule extends PreteToken

}


class PreteParsers extends RegexParsers {
  import Tokens._
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r
  val symbolRe = "[a-zA-Z_][a-zA-Z_0-9]*"
  val intRe = "[0-9]+"
  def string: Parser[String] = s""""$symbolRe"""".r ^^ { t => String(t.substring(1, t.length - 1)) }
  def symbol: Parser[Symbol] = symbolRe.r ^^ { x => Symbol(x) }
  def integer: Parser[Integer] = intRe.r ^^ { x => Integer(x.toInt) }
  def float: Parser[Float] = s"$intRe.$intRe".r ^^ { x => Float(x.toFloat) }
  def colon= ":".r ^^ { _ => Colon }
  def arrow = "=>".r ^^ { _ => Arrow }

  def indentation: Parser[IndentCount] =  "\n[ ]*".r ^^ { whitespace => IndentCount(whitespace.length - 1) }
  def defObject = "object".r ^^ { _ => DefObject }
  def defRule = "rule".r ^^ { _ => DefRule }

  val parsers = Seq(
    defObject,
    defRule,

    string,
    symbol,
    float,
    integer,
    colon,
    indentation,
  )

  def devour: Parser[List[PreteToken]] = {
    val head::tail = parsers
    phrase(
      rep1( tail.foldLeft(head)(_ | _) )
    ) ^^ { processIndentations(_) }
  }

  def processIndentations(tokens: List[PreteToken],
                          indents: List[Int] = List(0)): List[PreteToken] = {
    tokens.headOption match {

      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(IndentCount(spaces)) if spaces > indents.head =>
        Indent :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level,
      // producing a DEDENT for each pop
      case Some(IndentCount(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => Dedent)) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      // if the identation level is 0 then it's just a separation line
      case Some(IndentCount(spaces)) if spaces == 0 || spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => Dedent)

    }
  }
}

object PreteLexer extends PreteParsers {
  def apply(code: String): Either[PreteLexerError, List[PreteToken]] = {
    parse(devour, code) match {
      case NoSuccess(msg, next) => Left(PreteLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }
}