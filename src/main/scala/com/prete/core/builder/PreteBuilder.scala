package com.prete.core.builder

import com.prete.core.network.Blueprint
import com.prete.parser.{CompilationResult, PreteAST}

import scala.language.higherKinds

trait PreteBuilder[Token <: PreteAST, AST <: PreteAST] extends TokenizersCollection[Token] {
  def transformToToken[D, R >: PreteAST](data: D): CompilationResult[R]
  def blueprint(ast: AST): CompilationResult[Blueprint]
}


