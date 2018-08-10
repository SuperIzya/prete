package com.prete.core

import com.prete.core.builder.OperationContainer
import com.prete.core.network.Blueprint
import com.prete.parser.{PreteAST, PreteCompilationError, PreteToken}

package object command {

  case class CommandCompilationError(command: String, msg: String) extends PreteCompilationError
  case class CommandCallBlueprint(command: String, args: List[Any]) extends Blueprint

  case class CommandCallToken(command: String) extends PreteToken with OperationContainer
  case class CommandCall(command: String, args: List[PreteAST]) extends PreteAST with OperationContainer


}
