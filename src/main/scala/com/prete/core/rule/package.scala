package com.prete.core

import com.prete.core.network.Blueprint
import com.prete.parser._

package object rule {

  trait Condition extends Blueprint
  trait AlphaCondition extends Condition
  trait BetaCondition extends Condition

  case class RuleDefinition(name: String, lhs: List[PreteAST], rhs: List[PreteAST]) extends PreteAST
  case class ConstCompare(predicate: PreteAST, left: GetField, right: Value) extends PreteAST
  case class FieldsCompare(predicate: PreteAST, left: GetField, right: GetField) extends PreteAST
  case class StaticCompare(predicate: PreteAST, left: Value, right: Value) extends PreteAST

  case class RuleParseError(msg: String) extends Throwable with PreteCompilationError

  case class RuleCompilationError(rule: String, msg: String) extends PreteCompilationError
}
