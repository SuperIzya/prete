package com.prete.parser

import scala.util.parsing.input.{NoPosition, Position, Reader}

class PreteTokenReader(tokens: Seq[PreteToken]) extends Reader[PreteToken] {
  override def first: PreteToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PreteToken] = new PreteTokenReader(tokens.tail)
}