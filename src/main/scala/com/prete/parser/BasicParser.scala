package com.prete.parser

import scala.util.parsing.combinator.Parsers


trait BasicParser extends Parsers with PreteTokenParser {
  import Tokens._
  type ParserTree = Parser[PreteAST]
  type ParserForest = Parser[List[PreteAST]]

  def identifier: Parser[Symbol] = {
    accept("identifier", { case id @ Symbol(_) => id })
  }

  def literal: Parser[Value] = {
    accept("string literal", { case lit @ Text(_) => Value(lit) })
  }

  def float: Parser[Value] = {
    accept("float", { case num @ Float(_) => Value(num)})
  }
  def integer: Parser[Value] = {
    accept("integer", { case num @ Integer(_) => Value(num) })
  }
  def staticValue: Parser[Value] = integer | float | literal

  def fieldAddress: Parser[GetField] = identifier ~ Dot ~ identifier ^^ {
    case Symbol(name) ~ _ ~ Symbol(field) => GetField(name, field)
  }

  def argument: ParserTree = staticValue | fieldAddress

  def argumentWithComa: ParserTree = argument <~ Comma ^^ (arg => arg)
  def argumentsVertical: ParserForest = Indent ~> rep(argument) <~ Dedent ^^ (args => args)
  def argumentsHorizontal: ParserForest =
    OpenBr ~> rep(argumentWithComa) ~ argument <~ CloseBr ^^ {
      case args ~ arg => args :+ arg
    }
  def arguments: ParserForest = argumentsHorizontal | argumentsVertical
}
