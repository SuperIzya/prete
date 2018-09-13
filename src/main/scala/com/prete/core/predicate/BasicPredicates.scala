package com.prete.core.predicate

import com.prete.core.network.Blueprint
import com.prete.parser.PreteAST

object BasicPredicates extends PredicatesBuilder {

  case object Eq extends PreteAST with Predicate
  case object Neq extends PreteAST with Predicate
  case object Lt extends PreteAST with Predicate
  case object Le extends PreteAST with Predicate
  case object Gt extends PreteAST with Predicate
  case object Ge extends PreteAST with Predicate

  tokenizers = Map(
      "eq" -> { _ => Eq },
      "neq" -> { _ => Neq },
//      ">=" -> { _ => Ge },
//      ">" -> { _ => Gt },
//      "<=" -> { _ => Le },
//      "<" -> { _ => Lt },
    )

  def getPredicate: PartialFunction[Predicate, List[Any] => Boolean] = {
    case Eq => x => x(0) == x(1)
    case Neq => x => x(0) != x(1)
  }



}
