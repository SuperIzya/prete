package com.prete.core.command

import com.prete.core.builder.PreteBuilder
import com.prete.parser.{CompilationResult, PreteAST}


trait CommandsBuilder extends PreteBuilder[CommandCallAST, CommandCall] {
  // TODO: Define a body
  override def blueprint(ast: CommandCall) =
    Left(CommandCompilationError("Nope", ""))

  def callToken(cmd: String) = CommandCallAST(cmd)

  override def transformToToken[D, R >: PreteAST](data: D): CompilationResult[R] = data match {
    case Symbol(name) => Right(CommandCallAST(name))
    case _ => Left(
      CommandCompilationError("Unknown", s"Provided argument of type ${data.getClass} was not as expected: prete.Tokens.Symbol")
    )
  }
}
