package com.prete.core.builder

import com.prete.parser.{PreteAST, PreteToken}


trait BuildersRegistry[Builder <: PreteBuilder[Token, AST], -Token <: PreteToken, +AST <: PreteAST] {
  protected var builders = List.empty[Builder]

  def addBuilder[B <: Builder](builder: B): Unit = {
    builders :+= builder
  }

  def apply(builder: Builder): Unit = addBuilder(builder)

  def getTokenizers[T <: Token]: Map[String, String => T] =  builders
    .flatMap(x => x.getTokenizers.map{ ((y: Token) => x.token(y), _) })
    .map{ x => (x._2._1, (s: String) => x._1(x._2._2(s))) }
    .toMap

}
