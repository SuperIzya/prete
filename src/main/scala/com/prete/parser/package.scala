package com.prete

import scala.util.parsing.combinator.Parsers

package object parser {

  trait PreteAST
  trait WithValue[T] { this: PreteAST =>
    val value: T
  }
  trait PreteTokenParser extends Parsers {
    override type Elem = PreteAST
  }

  type CompilationResult[Result] = Either[PreteCompilationError, Result]

  case class Location(line: Int, column: Int) {
    override def toString = s"$line:$column"
  }
  trait PreteCompilationError
  case class PreteLexerError(location: Location, msg: String) extends PreteCompilationError
  case class PreteParserError(location: Location, msg: String) extends PreteCompilationError


  case class GetField(objName: String, field: String) extends PreteAST
  case class Value(value: WithValue[_]) extends PreteAST


}
