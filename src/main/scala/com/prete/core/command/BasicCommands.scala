package com.prete.core.command
import com.prete.core.network.Blueprint
import com.prete.parser.PreteCompilationError



object BasicCommands extends CommandsBuilder {
  override var tokenizers: TokenizersMap = List(
    "print",
    "debug"
  ).map(cmd =>
    cmd -> ((_: String) => callToken(cmd))
  ).toMap


  // TODO: Define a body
  def blueprint(ast: CommandCall) =
    Left(CommandCompilationError("Nope", ""))



}
