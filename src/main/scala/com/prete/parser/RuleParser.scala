package com.prete.parser

import scala.util.parsing.combinator.Parsers

trait RuleParser extends Parsers with PreteTokenParser with BlockParser with BaseParser {
  import Tokens._

  def command: Parser[PreteAST] = null
  def condition: Parser[PreteAST] = (introduction | atomicCondition) ^^ {
    case condition => condition
  }

  def atomicCondition =
    (staticValue | fieldAddress) ~ (Eq | Neq | Gt | Ge | Lt | Le) ~ (staticValue | fieldAddress) ^^ {
    case (v: Value) ~ op ~ (fa: FieldAddress) => AlphaCondition(op, fa, v)
    case (fa: FieldAddress) ~ op ~ (v: Value) => AlphaCondition(op, fa, v)
    case (v1: Value) ~ op ~ (v2: Value) => StaticCondition(op, v1, v2)
    case (fa1: FieldAddress) ~ op ~ (fa2: FieldAddress) => BetaCondition(op, fa1, fa2)
  }
  def introduction: Parser[Introduction] = identifier ~ BackArrow ~ identifier ^^ {
    case Symbol(name) ~ _ ~ Symbol(typeName) => Introduction(name, typeName)
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
