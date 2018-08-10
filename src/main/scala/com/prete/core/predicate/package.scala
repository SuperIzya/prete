package com.prete.core

import com.prete.core.builder.OperationContainer
import com.prete.core.network.Blueprint
import com.prete.core.rule.{AlphaCondition, Condition}
import com.prete.parser.{PreteAST, PreteCompilationError, PreteToken}

package object predicate {
  case class PredicateCompilationError(predicate: String, msg: String) extends PreteCompilationError

  trait Predicate extends Condition
  type PredicateTokenType = Predicate with PreteToken
  case class PredicateClause(predicate: Predicate, args: List[Any]) extends PreteAST
  case class PredicateToken(predicate: Predicate) extends Predicate with PreteToken with OperationContainer
  case class VarDeclaration(name: String, typeName: String) extends PreteAST with AlphaCondition

  case class PredicateBlueprint(predicate: Predicate, args: List[Blueprint]) extends Blueprint


}
