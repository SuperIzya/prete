package com.prete.core.builder

import com.prete.core.network.Blueprint
import com.prete.parser.{CompilationResult, PreteAST, PreteToken}

import scala.language.higherKinds

trait PreteBuilder[Token <: PreteToken, AST <: PreteAST] extends TokenizersCollection[Token] {
  def transformToToken[D, R >: PreteToken](data: D): CompilationResult[R]
  def blueprint(ast: AST): CompilationResult[Blueprint]
}


