package com.prete.core.fact

import com.prete.parser.{BasicParser, BlockParser, PreteAST, PreteTokenParser}

import scala.util.parsing.combinator.Parsers

case class FieldDefinition(name: String) extends PreteAST
case class FactDefinition(name: String,
                          fields: List[FieldDefinition]) extends PreteAST

trait DefFactParser extends Parsers
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