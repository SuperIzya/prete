package com.prete.core

import com.prete.core.network.Blueprint
import com.prete.parser.PreteCompilationError

import scala.language.higherKinds

trait PreteBuilder[-Token, +AST] {
  type TokenizersMap[U <: Token, B <: String => Token] = Map[String, String => U]
  protected var tokenizers: TokenizersMap[_, _] = Map.empty
  def getTokenizers: TokenizersMap[_, _] = tokenizers
  def token[D <: Token, R <: Token](data: D): R
  def blueprint[A >: AST, B <: Blueprint](ast: A): Either[PreteCompilationError, B]
}


trait OperationContainer

trait BuildersRegistry[BuilderType[-Token, +AST] <: PreteBuilder[Token, AST]] {
  protected var builders = List.empty[BuilderType[_, _]]

  def addBuilder(builder: BuilderType[_, _]): Unit = {
    builders :+= builder
  }

  def apply(builder: BuilderType[_, _]) = addBuilder(builder)

  def getTokenizers =  builders
    .flatMap(x => x.getTokenizers.map{ ((y: String) => x.token(y), _) })
    .map{ x => (x._2._1, (s: String) => x._1(x._2(s))) }
    .toMap

}
