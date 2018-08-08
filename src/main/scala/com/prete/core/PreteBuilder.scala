package com.prete.core

trait PreteBuilder[Token, AST] {
  type TokenizersMap = Map[String, String => Token]
  protected var tokenizers: TokenizersMap = Map.empty
  def getTokenizers: TokenizersMap = tokenizers
}
