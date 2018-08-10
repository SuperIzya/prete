package com.prete.core.predicate

import com.prete.core.network.Blueprint
import com.prete.parser.PreteToken

object BasicPredicates extends PredicatesBuilder {

  type Compiler = PartialFunction[Predicate, Either[PredicateCompilationError, Blueprint]]

  case object Eq extends PreteToken with Predicate
  case object Neq extends PreteToken with Predicate
  case object Lt extends PreteToken with Predicate
  case object Le extends PreteToken with Predicate
  case object Gt extends PreteToken with Predicate
  case object Ge extends PreteToken with Predicate

  override var tokenizers: TokenizersMap = Map(
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

  def blueprint(predicate: Predicate, args: List[Any]): Compiler =
    predicate match {
      case Eq | Neq | Lt | Gt | Le | Ge => null // TODO !!!

    }


}
