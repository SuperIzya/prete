package com.prete.core.command

import com.prete.core.builder.PreteBuilder
import com.prete.parser.PreteToken


trait CommandsBuilder extends PreteBuilder[CommandCallToken, CommandCall] {
  // TODO: Define a body
  override def blueprint(ast: CommandCall) =
    Left(CommandCompilationError("Nope", ""))

  def callToken(cmd: String) = CommandCallToken(cmd)

  override def transformToToken[D, R >: PreteToken](data: D): R = data match {
    case Symbol(name) => CommandCallToken(name)
    case CommandCallToken(name) => CommandCallToken(name)
  }
}
