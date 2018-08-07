package com.prete.predicate

import com.prete.Context
import com.prete.parser.PreteToken

object BasicPredicates extends PredicatesBuilder {

  case object Eq extends PreteToken with Predicate {
    def not: Predicate = Neq
  }
  case object Neq extends PreteToken with Predicate {
    def not: Predicate = Eq
  }
  case object Lt extends PreteToken with Predicate {
    def not: Predicate = Ge
  }
  case object Le extends PreteToken with Predicate {
    def not: Predicate = Gt
  }
  case object Gt extends PreteToken with Predicate {
    def not: Predicate = Le
  }
  case object Ge extends PreteToken with Predicate {
    def not: Predicate = Lt
  }

  private val tokenizers = Map(
      "eq" -> { _: Any => Eq },
      "neq" -> { _: Any => Neq },
//      ">=" -> { _ => Ge },
//      ">" -> { _ => Gt },
//      "<=" -> { _ => Le },
//      "<" -> { _ => Lt },
    )


  def inject(context: Context) = {
    context.tokenizer.addTokenizers(tokenizers)
  }

  def getLexers = tokenizers

  def getPredicate: PartialFunction[Predicate, List[Any] => Boolean] = {
    case Eq => x => x(0) == x(1)
    case Neq => x => x(0) != x(1)
  }
}
