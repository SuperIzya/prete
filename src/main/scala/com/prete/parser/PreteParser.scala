package com.prete.parser

import java.util.function.Predicate

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}

class PreteTokenReader(tokens: Seq[PreteToken]) extends Reader[PreteToken] {
  override def first: PreteToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PreteToken] = new PreteTokenReader(tokens.tail)
}

sealed trait PreteAST
case class FieldAddress(objName: String, field: String) extends PreteAST
case class Value(value: WithValue[_]) extends PreteAST

trait PreteTokenParser extends Parsers {
  override type Elem = PreteToken
}

trait BaseParser extends Parsers with PreteTokenParser {
  import Tokens._
  def identifier: Parser[Symbol] = {
    accept("identifier", { case id @ Symbol(_) => id })
  }

  def literal: Parser[Value] = {
    accept("string literal", { case lit @ String(_) => Value(lit) })
  }

  def float: Parser[Value] = {
    accept("float", { case num @ Float(_) => Value(num)})
  }
  def integer: Parser[Value] = {
    accept("integer", { case num @ Integer(_) => Value(num) })
  }
  def staticValue: Parser[Value] = integer | float | literal

  def fieldAddress: Parser[FieldAddress] = identifier ~ Dot ~ identifier ^^ {
    case Symbol(name) ~ _ ~ Symbol(field) => FieldAddress(name, field)
  }
}

trait BlockParser extends Parsers with PreteTokenParser {
  import Tokens._

  def optBlock[TypeAST](P: Parser[TypeAST]): Parser[List[TypeAST]] =
    opt(Indent ~ rep1(P) ~ Dedent) ^^ {
      case None => List.empty
      case Some(value) => value match {
        case _ ~ vals ~ _ => vals
      }
    }

  def trimBlock[TypeAST](P: Parser[TypeAST]): Parser[TypeAST] =
    rep(Indent | Dedent) ~ P ~ rep(Indent | Dedent) ^^ {
      case _ ~ vals ~ _ => vals
    }

}

case class FieldDefinition(name: String) extends PreteAST
case class ObjectDefinition(name: String, fields: List[FieldDefinition] = List.empty) extends PreteAST

case class RuleDefinition(name: String, lhs: List[PreteAST], rhs: List[PreteAST]) extends PreteAST
case class Declaration(name: String, typeName: String) extends PreteAST
case class ConstCompare(predicate: PreteToken, left: FieldAddress, right: Value) extends PreteAST
case class FieldsCompare(predicate: PreteToken, left: FieldAddress, right: FieldAddress) extends PreteAST
case class StaticCompare(predicate: PreteToken, left: Value, right: Value) extends PreteAST
