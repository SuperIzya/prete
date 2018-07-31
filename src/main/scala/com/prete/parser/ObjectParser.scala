package com.prete.parser

import scala.util.parsing.combinator.Parsers

trait DefObjectParser extends Parsers
  with PreteTokenParser
  with BaseParser
  with BlockParser {
  import Tokens._

  def defField: Parser[FieldDefinition] = {
    identifier ^^ { case Symbol(name) => FieldDefinition(name) }
  }

  def defObject: Parser[ObjectDefinition] = {
    (DefObject ~ identifier ~ optBlock(defField)) ^^ {
      case _ ~ Symbol(name) ~ fields => ObjectDefinition(name, fields)
    }
  }
}