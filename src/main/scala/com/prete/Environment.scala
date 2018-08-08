package com.prete

import com.prete.core.command.{BasicCommands, CommandsRegistry}
import com.prete.parser._
import com.prete.core.predicate.{BasicPredicates, PredicatesRegistry}

class Environment {
  val context = Context(
    PreteTokenizer(),
    PreteParser(),
    PredicatesRegistry(),
    CommandsRegistry()
  )
  protected def init(): Unit = {
    context.predicatesRegistry(BasicPredicates)
    context.commandsRegistry(BasicCommands)

    initTokenizers()
  }

  protected def initTokenizers(): Unit = {
    val allTokenizers = List(
      context.commandsRegistry.getTokenizers,
      context.predicatesRegistry.getTokenizers
    ).foldLeft(Map.empty[String, String => PreteToken])(_ ++ _)

    context.tokenizer.addTokenizers(allTokenizers)
  }

  def parse(code: String): Either[PreteCompilationError, List[PreteAST]] = {
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