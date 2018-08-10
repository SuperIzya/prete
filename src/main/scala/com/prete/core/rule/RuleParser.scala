package com.prete.core.rule
import com.prete.core.command.CommandParser
import com.prete.core.predicate.PredicatesParser
import com.prete.parser.{BasicParser, BlockParser, PreteTokenParser}

import scala.util.parsing.combinator.Parsers

trait RuleParser extends Parsers
  with PreteTokenParser
  with BlockParser
  with BasicParser
  with CommandParser
  with PredicatesParser {
  import com.prete.parser.Tokens._


  def defRule: Parser[RuleDefinition] = {
    DefRule ~ identifier ~
      optBlock(clause) ~
    Arrow ~
      optBlock(command) ^^ {
      case _ ~ Symbol(name) ~ lhs ~ _ ~ rhs => RuleDefinition(name, lhs, rhs)
    }
  }
}
