package com.prete.parser

import com.prete.core.fact.FactParser
import com.prete.core.rule.RuleParser

import scala.util.parsing.combinator._


class PreteParser extends Parsers
  with BlockParser
  with BasicParser
  with FactParser
  with RuleParser {

  def block: ParserForest = phrase(
    rep(trimBlock(defFact)) ~ rep(trimBlock(defRule)) ^^ {
      case objs ~ rules => objs ++ rules
    }
  )

  def program : ParserForest = block
  def apply(tokens: Seq[PreteAST]): Either[PreteParserError, List[PreteAST]] = {
    val reader = new PreteTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(PreteParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }
}

object PreteParser {
  def apply(): PreteParser = new PreteParser()
}
