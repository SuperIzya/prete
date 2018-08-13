package com.prete.core.builder

import com.prete.parser.{CompilationResult, PreteAST, PreteToken}


trait BuildersRegistry[Builder <: PreteBuilder[Token, AST], Token <: PreteToken, AST <: PreteAST] {
  protected var builders = List.empty[Builder]
  type Tokenizer = String => Token

  def addBuilder[B <: Builder](builder: B): Unit = {
    builders :+= builder
  }

  def apply(builder: Builder): Unit = addBuilder(builder)

  def getTokenizers: Map[String, String => CompilationResult[PreteToken]] =  builders
    .flatMap(x => x.getTokenizers.map{ ((y: Token) => x.transformToToken(y), _) })
    .map{ x => (x._2._1, (s: String) => x._1(x._2._2(s))) }
    .toMap

}
