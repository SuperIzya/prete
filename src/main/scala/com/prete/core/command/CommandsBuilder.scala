package com.prete.core.command

import com.prete.core.builder.PreteBuilder
import com.prete.parser.{CompilationResult, PreteToken}


trait CommandsBuilder extends PreteBuilder[CommandCallToken, CommandCall] {
  // TODO: Define a body
  override def blueprint(ast: CommandCall) =
    Left(CommandCompilationError("Nope", ""))

  def callToken(cmd: String) = CommandCallToken(cmd)

  override def transformToToken[D, R >: PreteToken](data: D): CompilationResult[R] = data match {
    case Symbol(name) => Right(CommandCallToken(name))
    case _ => Left(
      CommandCompilationError("Unknown", s"Provided argument of type ${data.getClass} was not as expected: prete.Tokens.Symbol")
    )
  }
}
