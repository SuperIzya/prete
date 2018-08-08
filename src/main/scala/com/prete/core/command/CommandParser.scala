package com.prete.core.command

import com.prete.core.fact.FactParser
import com.prete.parser.Tokens.Symbol
import com.prete.parser.{BasicParser, PreteAST, PreteToken}

case class CommandCallToken(command: String) extends PreteToken
case class CommandCall(command: String, args: List[PreteAST]) extends PreteAST

trait CommandParser extends BasicParser with FactParser {

  def command: Parser[CommandCall] = identifier ~ arguments ^^ {
    case Symbol(command) ~ args => CommandCall(command, args)
  }
}
