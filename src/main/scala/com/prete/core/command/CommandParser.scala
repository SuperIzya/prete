package com.prete.core.command

import com.prete.core.fact.FactParser
import com.prete.parser.BasicParser
import com.prete.parser.Tokens.Symbol

trait CommandParser extends BasicParser with FactParser {

  def command: Parser[CommandCall] = identifier ~ arguments ^^ {
    case Symbol(command) ~ args => CommandCall(command, args)
  }
}
