package com.prete.parser

import scala.util.parsing.combinator.RegexParsers


sealed trait PreteCompilationError
case class PreteLexerError(location: Location, msg: String) extends PreteCompilationError
case class PreteParserError(location: Location, msg: String) extends PreteCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}


sealed trait PreteToken
trait WithValue[T] { this: PreteToken =>
  val value: T
}
object Tokens {

  private type _String = java.lang.String
  private type _Integer = java.lang.Integer
  private type _Float = java.lang.Float

  case class Symbol(value: _String) extends PreteToken with WithValue[_String]
  case class Integer(value: Int) extends PreteToken with WithValue[Int]
  case class Float(value: _Float) extends PreteToken with WithValue[_Float]
  case class String(value: _String) extends PreteToken with WithValue[_String]

  case object Colon extends PreteToken
  case object OpenBracket extends PreteToken
  case object CloseBracket extends PreteToken

  case class IndentCount(length: Int) extends PreteToken
  case object Dedent extends PreteToken
  case object Indent extends PreteToken
  case object Arrow extends PreteToken
  case object BackArrow extends PreteToken
  case object Dot extends PreteToken
  case object Eq extends PreteToken
  case object Neq extends PreteToken
  case object Lt extends PreteToken
  case object Le extends PreteToken
  case object Gt extends PreteToken
  case object Ge extends PreteToken

  case object DefObject extends PreteToken
  case object DefRule extends PreteToken

}


class PreteParsers extends RegexParsers {
  import Tokens._
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r
  val symbolRe = "[a-zA-Z_][a-zA-Z_0-9]*"
  val intRe = "[0-9]+"
  val parsers = List[Parser[PreteToken]](
    "object".r ^^ { _ => DefObject },
    "rule".r ^^ { _ => DefRule },
    s""""$symbolRe"""".r ^^ { t => String(t.substring(1, t.length - 1)) },
    symbolRe.r ^^ { x => Symbol(x) },
    s"(\\+|\\-)?$intRe.$intRe".r ^^ { x => Float(x.toFloat) },
    s"(\\+|\\-)?$intRe".r ^^ { x => Integer(x.toInt) },

    "\n[ ]*".r ^^ { whitespace => IndentCount(whitespace.length - 1) },


    "=>".r ^^ { _ => Arrow },
    ":".r ^^ { _ => Colon },
    "<-".r ^^ { _ => BackArrow },

    "==".r ^^ { _ => Eq },
    "!=".r ^^ { _ => Neq },
    ">=".r ^^ { _ => Ge },
    ">".r ^^ { _ => Gt },
    "<=".r ^^ { _ => Le },
    "<".r ^^ { _ => Lt },

    ".".r ^^ { _ => Dot },
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