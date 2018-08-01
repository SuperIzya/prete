package com.prete.parser

import scala.util.parsing.combinator.Parsers

trait RuleParser extends Parsers with PreteTokenParser with BlockParser with BaseParser {
  import Tokens._

  def command: Parser[PreteAST] = null
  def condition: Parser[PreteAST] = (declaration | comparison) ^^ {
    case condition => condition
  }

  def comparison: Parser[PreteAST] =
    (staticValue | fieldAddress) ~ (Eq | Neq | Gt | Ge | Lt | Le) ~ (staticValue | fieldAddress) ^^ {
    case (v: Value) ~ op ~ (fa: FieldAddress) => ConstCompare(op, fa, v)
    case (fa: FieldAddress) ~ op ~ (v: Value) => ConstCompare(op, fa, v)
    case (v1: Value) ~ op ~ (v2: Value) => StaticCompare(op, v1, v2)
    case (fa1: FieldAddress) ~ op ~ (fa2: FieldAddress) => FieldsCompare(op, fa1, fa2)
  }
  def declaration: Parser[Declaration] = identifier ~ BackArrow ~ identifier ^^ {
    case Symbol(name) ~ _ ~ Symbol(typeName) => Declaration(name, typeName)
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
