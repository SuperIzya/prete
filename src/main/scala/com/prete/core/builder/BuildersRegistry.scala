package com.prete.core.builder

import com.prete.parser.{CompilationResult, PreteAST}


trait BuildersRegistry[Builder <: PreteBuilder[Token, AST], Token, AST <: PreteAST] {
  protected var builders = List.empty[Builder]
  type Tokenizer = String => Token

  def addBuilder[B <: Builder](builder: B) = {
    builders :+= builder
    this
  }

  def apply(builder: Builder) = addBuilder(builder)

  def getTokenizers: Map[String, String => CompilationResult[PreteAST]] =  builders
    .flatMap(x => x.getTokenizers.map{ (x.transformToToken(_), _) })
    .map{ x => (x._2._1, (s: String) => x._1(x._2._2(s))) }
    .toMap

}
