package com.prete.parser

import com.prete.core.fact.DefFactParser
import com.prete.core.rule.RuleParser

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}

case class PreteParserError(location: Location, msg: String) extends PreteCompilationError

class PreteTokenReader(tokens: Seq[PreteToken]) extends Reader[PreteToken] {
  override def first: PreteToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PreteToken] = new PreteTokenReader(tokens.tail)
}

trait PreteAST
case class FieldAddressAST(objName: String, field: String) extends PreteAST
case class Value(value: WithValue[_]) extends PreteAST

trait PreteTokenParser extends Parsers {
  override type Elem = PreteToken
}

trait BasicParser extends Parsers with PreteTokenParser {
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

  def fieldAddress: Parser[FieldAddressAST] = identifier ~ Dot ~ identifier ^^ {
    case Symbol(name) ~ _ ~ Symbol(field) => FieldAddressAST(name, field)
  }

  def argument: Parser[PreteAST] = staticValue | fieldAddress

  def arguments: Parser[List[PreteAST]] =
    Indent ~ rep1(argument) ~ Dedent ^^ {
      case _ ~ args ~ _ => args
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


trait PreteParser extends Parsers
  with BlockParser
  with BasicParser
  with DefFactParser
  with RuleParser {

  def block: Parser[List[PreteAST]] = phrase(
    rep(trimBlock(defFact)) ~ rep(trimBlock(defRule)) ^^ {
      case objs ~ rules => objs ++ rules
    }
  )

  def program : Parser[List[PreteAST]] = block
  def apply(tokens: Seq[PreteToken]): Either[PreteParserError, List[PreteAST]] = {
    val reader = new PreteTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(PreteParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }
}

object PreteParser extends PreteParser
