package com.prete.parser

import scala.util.parsing.combinator.RegexParsers



class PreteTokenizer extends RegexParsers {
  import Tokens._
  type PreteTokenParser = Parser[PreteToken]
  type PreteParserFunction = _String => PreteToken
  type TokenizerPair = (_String, PreteParserFunction)
  type TokenizersMap = Map[_String, PreteParserFunction]

  val whiteSpaceRe = """[ \t\r\f]+"""
  val symbolRe = "[a-zA-Z_][a-zA-Z_0-9]*"
  val stringRe = s""""[^"]*""""
  val numRe = "[0-9]+"
  val floatRe = s"""$numRe.$numRe"""

  override def skipWhitespace = true
  override val whiteSpace = whiteSpaceRe.r
  protected var additionalTokenizers: TokenizersMap = Map.empty
  protected val coreTokenizers = List(
    "fact" ^^ { _ => DefFact },
    "rule" ^^ { _ => DefRule },

    "=>" ^^ { _ => Arrow },
    ":" ^^ { _ => Colon },
    "<-" ^^ { _ => BackArrow },
  )
  protected val basicTokenizers = List(

    stringRe.r ^^ { t => String(t.substring(1, t.length - 1)) },
    symbolRe.r ^^ { x => Symbol(x) },
    s"(\\+|\\-)?$floatRe".r ^^ { x => Float(x.toFloat) },
    s"(\\+|\\-)?$numRe".r ^^ { x => Integer(x.toInt) },

    "\n[ ]*".r ^^ { whitespace => IndentCount(whitespace.length - 1) },

    "(" ^^ { _ => OpenBr },
    ")" ^^ { _ => CloseBr },

    "." ^^ { _ => Dot },
    "," ^^ { _ => Comma },
    whiteSpaceRe ^^ { _ => Whitespace }
  )

  private def toTokenizer(f: TokenizerPair): Parser[PreteToken] =
    s"${f._1}".r ^^ f._2

  def addTokenizer(regex: _String, func: PreteParserFunction): PreteTokenizer = {
    additionalTokenizers = additionalTokenizers + (regex -> func)
    this
  }
  def addTokenizers(l: TokenizersMap): PreteTokenizer = {
    additionalTokenizers = additionalTokenizers ++ l
    this
  }

  def apply(l: TokenizersMap) = addTokenizers(l)

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
