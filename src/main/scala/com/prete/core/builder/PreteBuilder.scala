package com.prete.core.builder

import com.prete.core.network.Blueprint
import com.prete.parser.{PreteAST, PreteCompilationError, PreteToken}

import scala.language.higherKinds

trait PreteBuilder[-Token <: PreteToken, +AST <: PreteAST] extends TokenizersCollection[PreteToken] {
  def token[R <: Token](data: Token): R
  def blueprint[A >: AST <: PreteAST](ast: A): Either[PreteCompilationError, Blueprint]
}


