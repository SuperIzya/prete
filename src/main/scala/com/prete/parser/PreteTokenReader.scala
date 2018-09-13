package com.prete.parser

import scala.util.parsing.input.{NoPosition, Position, Reader}

class PreteTokenReader(tokens: Seq[PreteAST]) extends Reader[PreteAST] {
  override def first: PreteAST = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PreteAST] = new PreteTokenReader(tokens.tail)
}