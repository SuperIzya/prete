package com.prete.core.rule
import com.prete.parser.{BasicParser, BlockParser, FieldAddressAST, PreteAST, PreteToken, PreteTokenParser, Value}
import com.prete.predicate.PredicatesParser

import scala.util.parsing.combinator.Parsers

case class RuleDefinition(name: String, lhs: List[PreteAST], rhs: List[PreteAST]) extends PreteAST
case class VarDeclaration(name: String, typeName: String) extends PreteAST
case class ConstCompare(predicate: PreteToken, left: FieldAddressAST, right: Value) extends PreteAST
case class FieldsCompare(predicate: PreteToken, left: FieldAddressAST, right: FieldAddressAST) extends PreteAST
case class StaticCompare(predicate: PreteToken, left: Value, right: Value) extends PreteAST

trait RuleParser extends Parsers
  with PreteTokenParser
  with BlockParser
  with BasicParser
  with PredicatesParser {
  import com.prete.parser.Tokens._

  def command: Parser[PreteAST] = null
  def condition: Parser[PreteAST] = (declaration | predicate) ^^ { x => x }

  def declaration: Parser[VarDeclaration] = identifier ~ BackArrow ~ identifier ^^ {
    case Symbol(name) ~ _ ~ Symbol(typeName) => VarDeclaration(name, typeName)
  }

  def defRule: Parser[RuleDefinition] = {
    (DefRule ~ identifier ~
      optBlock(condition) ~
      Arrow ~
      optBlock(command) ) ^^ {
      case _ ~ Symbol(name) ~ lhs ~ _ ~ rhs => RuleDefinition(name, lhs, rhs)
    }
  }
}
