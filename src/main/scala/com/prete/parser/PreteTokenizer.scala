package com.prete.parser

import cats.instances.map

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers



class PreteTokenizer extends RegexParsers {
  import Tokens._
  type PreteTokenParser = Parser[CompilationResult[PreteAST]]
  type PreteParserFunction = String => PreteAST
  type TokenizerPair = (String, PreteParserFunction)
  type TokenizersMap = List[TokenizerPair]

  val whiteSpaceRe = """[ \t\r\f]+"""
  val symbolRe = "[a-zA-Z_][a-zA-Z_0-9]*"
  val stringRe = s""""[^"]*""""
  val numRe = "[0-9]+"
  val floatRe = s"""$numRe.$numRe"""

  override def skipWhitespace = true
  override val whiteSpace: Regex = whiteSpaceRe.r
  protected var additionalTokenizers: TokenizersMap = List.empty
  protected val coreTokenizers = List(
    "fact" ^^ { _ => DefFact },
    "rule" ^^ { _ => DefRule },

    "=>" ^^ { _ => Arrow },
    ":" ^^ { _ => Colon },
    "<-" ^^ { _ => BackArrow },
  )
  protected val basicTokenizers = List(
    stringRe.r ^^ { t => Text(t.substring(1, t.length - 1)) },
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

  private def toTokenizer(f: TokenizerPair): Parser[PreteAST] =
    s"${f._1}".r ^^ f._2

  def addTokenizer(regex: String, func: PreteParserFunction): PreteTokenizer = {
    additionalTokenizers :+= (regex -> func)
    this
  }
  def addTokenizers(l: TokenizersMap): PreteTokenizer = {
    additionalTokenizers ++= l
    this
  }
  def addTokenizers(l: Map[String, PreteParserFunction]): PreteTokenizer = {
    additionalTokenizers ++= l
    this
  }

  def apply(l: TokenizersMap): PreteTokenizer = addTokenizers(l)

  def wrap(l: List[Parser[PreteAST]]): List[PreteTokenParser] = {

    type Result = List[PreteTokenParser]

    @tailrec
    def rec(n: Int, acc: Result = List.empty): Result =
      if (n >= l.length) acc
      else rec(n + 1, acc :+ l.head.map(Right(_)))

    rec(0)
  }
  def tokenizers: List[PreteTokenParser] = wrap(
    coreTokenizers ++
    additionalTokenizers.map{ toTokenizer } ++
      basicTokenizers
    )

  def devour: Parser[List[PreteAST]] = {
    val head::tail = tokenizers
    phrase(
      rep1( tail.foldLeft(head)(_ | _) )
    ) ^^ { processIndentations(_) }
  }


  def processIndentations(tokens: List[PreteAST],
                          indents: List[Int] = List(0)): List[PreteAST] = {
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
      // if the indentation level is 0 then it's just a separation line
      case Some(IndentCount(spaces)) if spaces == 0 || spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => Dedent)

    } map
  }
  def apply(code: String): Either[PreteLexerError, List[PreteAST]] = {
    parse(devour, code) match {
      case NoSuccess(msg, next) => Left(PreteLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }
}

object PreteTokenizer {
  def apply(): PreteTokenizer = new PreteTokenizer()
}
