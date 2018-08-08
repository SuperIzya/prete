package com.prete
import com.prete.core.command.CommandsRegistry
import com.prete.core.fact.Fact
import com.prete.core.predicate.PredicatesRegistry
import com.prete.core.rule.Rule
import com.prete.parser.{PreteParser, PreteTokenizer}

case class Context(
                    tokenizer: PreteTokenizer,
                    parser: PreteParser,
                    predicatesRegistry: PredicatesRegistry,
                    commandsRegistry: CommandsRegistry,
                    factDefinitions: Map[String, Fact] = Map.empty,
                    ruleDefinitions: Map[String, Rule] = Map.empty
                  )
