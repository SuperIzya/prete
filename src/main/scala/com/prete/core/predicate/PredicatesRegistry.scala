package com.prete.core.predicate

import com.prete.core.builder.BuildersRegistry
import com.prete.parser.PreteToken

class PredicatesRegistry extends BuildersRegistry[PredicatesBuilder, Predicate with PreteToken, PredicateClause] {

  def getPredicate(predicate: Predicate) = {
    val l = builders.map( _.getPredicate )
    l.tail.foldLeft(l.head)( _ orElse _ )(predicate)
  }

}

object PredicatesRegistry {
  def apply(): PredicatesRegistry = new PredicatesRegistry()
}
