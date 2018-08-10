package com.prete.core.fact

import com.prete.parser.{BasicParser, BlockParser, PreteTokenParser}

import scala.util.parsing.combinator.Parsers


trait FactParser extends Parsers
  with PreteTokenParser
  with BasicParser
  with BlockParser {
  import com.prete.parser.Tokens._

  def defField: Parser[FieldDefinition] = {
    identifier ^^ { case Symbol(name) => FieldDefinition(name) }
  }

  def defFact: Parser[FactDefinition] = {
    (DefFact ~ identifier ~ optBlock(defField)) ^^ {
      case _ ~ Symbol(name) ~ fields => FactDefinition(name, fields)
    }
  }
}