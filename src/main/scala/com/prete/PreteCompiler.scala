package com.prete

import com.prete.parser.{PreteAST, PreteCompilationError, PreteLexer, PreteParser}

object PreteCompiler {
  val lexer: PreteLexer = PreteLexer
  val parser: PreteParser = PreteParser
  def apply(code: String): Either[PreteCompilationError, List[PreteAST]] = {
    for {
      tokens <- lexer(code).right
      ast <- parser(tokens).right
    } yield ast
  }



}
