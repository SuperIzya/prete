package com.prete.core.builder

import com.prete.parser.CompilationResult

trait TokenizersCollection[Token] {
  type Tokenizer = String => CompilationResult[Token]
  type TokenizersMap = Map[String, Tokenizer]

  protected var tokenizers: TokenizersMap = Map.empty

  def getTokenizers: TokenizersMap = tokenizers
}
