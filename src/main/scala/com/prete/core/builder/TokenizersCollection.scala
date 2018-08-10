package com.prete.core.builder

trait TokenizersCollection[+Token] {
  type Tokenizer = String => Token
  type TokenizersMap = Map[String, Tokenizer]

  protected var tokenizers: TokenizersMap = Map.empty

  def getTokenizers: TokenizersMap = tokenizers
}
