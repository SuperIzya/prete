package com.prete.core.command

object BasicCommands extends CommandsBuilder {
  tokenizers = List(
    "print",
    "debug"
  ).map(cmd =>
    cmd -> ((_: String) => callToken(cmd))
  ).toMap
}
