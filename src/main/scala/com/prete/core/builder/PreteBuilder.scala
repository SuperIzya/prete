package com.prete.core.builder

import com.prete.core.network.Blueprint
import com.prete.parser.{PreteAST, PreteCompilationError, PreteToken}

import scala.language.higherKinds

trait PreteBuilder[Token <: PreteToken, AST <: PreteAST] extends TokenizersCollection[Token] {
  def transformToToken[D, R >: PreteToken](data: D): R
  def blueprint(ast: AST): Either[PreteCompilationError, Blueprint]
}


