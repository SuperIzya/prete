package com.prete

import scala.util.parsing.combinator.Parsers

package object parser {

  trait PreteToken
  trait WithValue[T] { this: PreteToken =>
    val value: T
  }
  trait PreteTokenParser extends Parsers {
    override type Elem = PreteToken
  }

  type CompilationResult[Result] = Either[PreteCompilationError, Result]

  case class Location(line: Int, column: Int) {
    override def toString = s"$line:$column"
  }
  trait PreteCompilationError
  case class PreteLexerError(location: Location, msg: String) extends PreteCompilationError
  case class PreteParserError(location: Location, msg: String) extends PreteCompilationError


  trait PreteAST
  case class FieldAddressAST(objName: String, field: String) extends PreteAST
  case class Value(value: WithValue[_]) extends PreteAST


}
