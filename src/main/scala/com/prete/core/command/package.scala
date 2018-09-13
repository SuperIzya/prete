package com.prete.core

import com.prete.core.builder.OperationContainer
import com.prete.core.network.Blueprint
import com.prete.parser.{PreteAST, PreteCompilationError}

package object command {

  case class CommandCompilationError(command: String, msg: String) extends PreteCompilationError
  case class CommandCallBlueprint(command: String, args: List[Any]) extends Blueprint

  case class CommandCallAST(command: String) extends PreteAST with OperationContainer
  case class CommandCall(command: String, args: List[PreteAST]) extends PreteAST with OperationContainer


}
