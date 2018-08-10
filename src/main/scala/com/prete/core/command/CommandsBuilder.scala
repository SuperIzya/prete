package com.prete.core.command

import com.prete.core.builder.PreteBuilder
import com.prete.parser.{PreteToken, Tokens}


trait CommandsBuilder extends PreteBuilder[CommandCallToken, CommandCall] {

  def callToken(cmd: String) = CommandCallToken(cmd)

  override def token(data: PreteToken): CommandCallToken = data match {
    case token @ CommandCallToken(_) => token
    case Tokens.Symbol(name) => CommandCallToken(name)
    case _ => null
  }
}
