package com.prete.parser

import scala.util.parsing.combinator.Parsers

object PreteParser extends Parsers
  with BlockParser
  with BaseParser
  with DefObjectParser
  with RuleParser {

  def block: Parser[List[PreteAST]] = phrase(
    rep(trimBlock(defObject)) ~ rep(trimBlock(defRule)) ^^ {
      case objs ~ rules => objs ++ rules
    }
  )

  def program : Parser[List[PreteAST]] = block
  def apply(tokens: Seq[PreteToken]): Either[PreteParserError, List[PreteAST]] = {
    val reader = new PreteTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(PreteParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }
}
object PreteCompiler {
  def apply(code: String): Either[PreteCompilationError, List[PreteAST]] = {
    for {
      tokens <- PreteLexer(code).right
      ast <- PreteParser(tokens).right
    } yield ast
  }
}
