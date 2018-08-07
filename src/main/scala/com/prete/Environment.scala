package com.prete

import com.prete.parser.{PreteAST, PreteCompilationError, PreteTokenizer, PreteParser}
import com.prete.predicate.{BasicPredicates, PredicatesRegistry}

class Environment {
  val context = Context(PreteTokenizer(), PreteParser())
  protected def init(): Unit = {
    PredicatesRegistry.addBuilder(BasicPredicates)

    context.tokenizer.addTokenizers(PredicatesRegistry.getLexers)
  }

  def compile(code: String): Either[PreteCompilationError, List[PreteAST]] = {
    for {
      tokens <- context.tokenizer(code).right
      ast <- context.parser(tokens).right
    } yield ast
  }
}

object Environment {
  def apply(): Environment = {
    val env = new Environment()
    env.init()
    env
  }
}