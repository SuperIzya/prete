package com.prete.core.command
import com.prete.core.network.Blueprint

object BasicCommands extends CommandsBuilder {
  override var tokenizers: TokenizersMap[_, _] = List(
    "print",
    "debug"
  ).map(cmd =>
    cmd -> ((_: String) => CommandCallToken(cmd))
  ).toMap


  // TODO: Define a body
  def blueprint(ast: CommandCall) =
    Left(CommandCompilationError("Nope", ""))
}
