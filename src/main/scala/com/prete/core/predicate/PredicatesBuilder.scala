package com.prete.core.predicate

import com.prete.core.builder.PreteBuilder

trait PredicatesBuilder
  extends PreteBuilder[PredicateTokenType, PredicateClause] {

  def getPredicate: PartialFunction[Predicate, List[Any] => Boolean]

  override def transformToToken[D, R >: PredicateTokenType](data: D): PredicateTokenType =
    PredicateToken(data.asInstanceOf[Predicate])

  override def blueprint(ast: PredicateClause) =
    Left(PredicateCompilationError(ast.predicate.toString, "Compilation not implemented yet"))
}