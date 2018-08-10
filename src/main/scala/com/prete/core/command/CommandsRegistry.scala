package com.prete.core.command

import com.prete.core.builder.BuildersRegistry

class CommandsRegistry extends BuildersRegistry[CommandsBuilder, CommandCallToken, CommandCall]

object CommandsRegistry {
  def apply() = new CommandsRegistry()
}
