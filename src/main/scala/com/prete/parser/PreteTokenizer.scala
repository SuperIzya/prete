package com.prete.parser

import scala.util.parsing.combinator.RegexParsers


trait PreteCompilationError
case class PreteLexerError(location: Location, msg: String) extends PreteCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}


trait PreteToken
trait WithValue[T] { this: PreteToken =>
  val value: T
}
object Tokens {

  type _String = java.lang.String
  type _Integer = java.lang.Integer
  type _Float = java.lang.Float

  case class Symbol(value: _String) extends PreteToken with WithValue[_String]
  case class Integer(value: Int) extends PreteToken with WithValue[Int]
  case class Float(value: _Float) extends PreteToken with WithValue[_Float]
  case class String(value: _String) extends PreteToken with WithValue[_String]

  case object Colon extends PreteToken

  case class IndentCount(length: Int) extends PreteToken
  case object Dedent extends PreteToken
  case object Indent extends PreteToken
  case object Arrow extends PreteToken
  case object BackArrow extends PreteToken
  case object Dot extends PreteToken

  case object Comma extends PreteToken
  case object OpenBr extends PreteToken
  case object CloseBr extends PreteToken

  case object DefFact extends PreteToken
  case object DefRule extends PreteToken

}


class PreteTokenizer extends RegexParsers {
  import Tokens._
  type PreteTokenParser = Parser[PreteToken]
  type PreteParserFunction = _String => PreteToken

  val whiteSpaceRe = """[ \t\r\f]+"""
  val symbolRe = "[a-zA-Z_][a-zA-Z_0-9]*"
  val stringRe = s""""${symbolRe}($whiteSpaceRe$symbolRe)*""""
  val intRe = "[0-9]+"

  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r
  protected var additionalTokenizers: Map[_String, PreteParserFunction] = Map.empty
  protected val coreTokenizers = List(
    "object".r ^^ { _ => DefFact },
    "rule".r ^^ { _ => DefRule },

    "=>".r ^^ { _ => Arrow },
    ":".r ^^ { _ => Colon },
    "<-".r ^^ { _ => BackArrow },
  )
  protected val basicTokenizers = List(

    stringRe.r ^^ { t => String(t.substring(1, t.length - 1)) },
    symbolRe.r ^^ { x => Symbol(x) },
    s"(\\+|\\-)?$intRe.$intRe".r ^^ { x => Float(x.toFloat) },
    s"(\\+|\\-)?$intRe".r ^^ { x => Integer(x.toInt) },

    "\n[ ]*".r ^^ { whitespace => IndentCount(whitespace.length - 1) },

    "(" ^^ { _ => OpenBr },
    ")" ^^ { _ => CloseBr },

    "." ^^ { _ => Dot },
    "," ^^ { _ => Comma },
  )

  private def toTokenizer(f: (_String, PreteParserFunction)): Parser[PreteToken] =
    s"${f._1}".r ^^ f._2

  def addTokenizer(regex: _String, func: PreteParserFunction): PreteTokenizer = {
    additionalTokenizers = additionalTokenizers + (regex -> func)
    this
  }
  def addTokenizers(l: Map[_String, PreteParserFunction]): PreteTokenizer = {
    additionalTokenizers = additionalTokenizers ++ l
    this
  }

  def tokenizers: List[PreteTokenParser] =
    coreTokenizers ++
    additionalTokenizers.map{ toTokenizer } ++
      basicTokenizers

  def devour: Parser[List[PreteToken]] = {
    val head::tail = tokenizers
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
  def apply(code: _String): Either[PreteLexerError, List[PreteToken]] = {
    parse(devour, code) match {
      case NoSuccess(msg, next) => Left(PreteLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }
}

object PreteTokenizer {
  def apply(): PreteTokenizer = new PreteTokenizer()
}
