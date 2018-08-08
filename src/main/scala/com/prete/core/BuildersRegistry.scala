package com.prete.core

trait OperationContainer

trait BuildersRegistry[Token <: new(), AST, BuilderType <: PreteBuilder[Token, AST]] {
  protected var builders = List.empty[BuilderType]

  def addBuilder(builder: BuilderType): Unit = {
    builders :+= builder
  }

  def apply(builder: BuilderType) = addBuilder(builder)

  def getTokenizers =  builders
    .flatMap( _.getTokenizers )
    .map{ x => (x._1, (s: String) => new Token(x._2(s))) }
    .toMap

}
