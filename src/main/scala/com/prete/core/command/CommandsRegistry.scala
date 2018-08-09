package com.prete.core.command

import com.prete.core.{BuildersRegistry, PreteBuilder}
import com.prete.parser.{PreteCompilationError, PreteToken, Tokens}

case class CommandCompilationError(command: String, msg: String) extends PreteCompilationError
trait CommandsBuilder extends PreteBuilder[CommandCallToken, CommandCall] {
  def token(data: PreteToken): CommandCallToken = data match {
    case token @ CommandCallToken(_) => token
    case Tokens.Symbol(name) => CommandCallToken(name)
  }
}

class CommandsRegistry extends BuildersRegistry[CommandsBuilder]

object CommandsRegistry {
  def apply() = new CommandsRegistry()
}
