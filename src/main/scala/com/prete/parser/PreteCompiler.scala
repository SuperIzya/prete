package com.prete.parser

object PreteCompiler {
  def apply(code: String): Either[PreteCompilationError, List[PreteAST]] = {
    for {
      tokens <- PreteLexer(code).right
      ast <- PreteParser(tokens).right
    } yield ast
  }
}
