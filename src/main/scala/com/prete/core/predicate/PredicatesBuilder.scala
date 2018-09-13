package com.prete.core.predicate

import com.prete.core.builder.PreteBuilder
import com.prete.parser.CompilationResult

trait PredicatesBuilder
  extends PreteBuilder[PredicateTokenType, PredicateClause] {

  def getPredicate: PartialFunction[Predicate, List[Any] => Boolean]

  override def transformToToken[D, R >: PredicateTokenType](data: D): CompilationResult[R] = {
    data match {
      case predicate: Predicate =>
        Right(
          PredicateAST(predicate)
        )
      case _ =>
        Left(
          PredicateCompilationError("Unknown", s"Provided argument of type ${data.getClass} is not of type Predicate")
        )
    }
  }
  override def blueprint(ast: PredicateClause) =
    Left(PredicateCompilationError(ast.predicate.toString, "Compilation not implemented yet"))
}