package com.prete.core.command

object BasicCommands extends CommandsBuilder {
  override var tokenizers: TokenizersMap = List(
    "print",
    "debug"
  ).map(cmd =>
    cmd -> ((_: String) => CommandCallToken(cmd))
  ).toMap


}
