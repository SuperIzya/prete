package com.prete.core.predicate

import com.prete.core.builder.PreteBuilder
import com.prete.parser.{PreteAST, PreteToken}

trait PredicatesBuilder
  extends PreteBuilder[PredicateTokenType, PredicateClause] {

  def getPredicate: PartialFunction[Predicate, List[Any] => Boolean]

  override def token(data: PredicateTokenType) = PredicateToken(data)

  override def blueprint(clause: PreteAST) =
    Left(PredicateCompilationError(clause.toString, ""))
}