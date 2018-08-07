package com.prete

import com.prete.parser.{PreteLexer, PreteParser}
import com.prete.predicate.{BasicPredicates, PredicatesRegistry}

class Environment {
  val context = Context(PreteLexer, PreteParser)
  protected def init(): Unit = {
    PredicatesRegistry.addBuilder(BasicPredicates)

    context.lexer.addLexer(PredicatesRegistry.getLexers)
  }
}

object Environment {
  def apply: Environment = {
    val env = new Environment()
    env.init()
    env
  }
}