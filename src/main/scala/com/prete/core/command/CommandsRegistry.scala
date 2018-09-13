package com.prete.core.command

import com.prete.core.builder.BuildersRegistry

class CommandsRegistry extends BuildersRegistry[CommandsBuilder, CommandCallAST, CommandCall]

object CommandsRegistry {
  def apply() = new CommandsRegistry()
}
