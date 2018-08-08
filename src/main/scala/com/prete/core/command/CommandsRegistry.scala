package com.prete.core.command

import com.prete.core.{BuildersRegistry, PreteBuilder}

trait CommandsBuilder extends PreteBuilder[CommandCallToken, CommandCall] {

}

class CommandsRegistry extends BuildersRegistry[CommandCallToken, CommandCall, CommandsBuilder]

object CommandsRegistry {
  def apply() = new CommandsRegistry()
}
