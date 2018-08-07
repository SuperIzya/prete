package com.prete
import com.prete.core.fact.Fact
import com.prete.core.rule.Rule
import com.prete.parser.{PreteLexer, PreteParser}

case class Context(
                  lexer: PreteLexer,
                  parser: PreteParser,
                  factDefinitions: Map[String, Fact] = Map.empty,
                  ruleDefinitions: Map[String, Rule] = Map.empty
                  )
