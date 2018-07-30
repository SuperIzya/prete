package com.prete.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}

class WorkflowTokenReader(tokens: Seq[PreteToken]) extends Reader[PreteToken] {
  override def first: PreteToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PreteToken] = new WorkflowTokenReader(tokens.tail)
}

sealed trait PreteAST
case class FieldDefinition(name: String) extends PreteAST
case class ObjectDefinition(name: String, fields: List[FieldDefinition] = List.empty) extends PreteAST

case class RuleDefinition(name: String, lhs: List[ConditionDefinition.type], rhs: List[CommandDefinition.type]) extends PreteAST
case object ConditionDefinition extends PreteAST
case object CommandDefinition extends PreteAST

object PreteParser extends Parsers {

  override type Elem = PreteToken
  import Tokens._

  private def identifier: Parser[Symbol] = {
    accept("identifier", { case id @ Symbol(_) => id })
  }

  private def literal: Parser[String] = {
    accept("string literal", { case lit @ String(_) => lit })
  }


  def defField: Parser[FieldDefinition] = {
    identifier ^^ { case Symbol(name) => FieldDefinition(name) }
  }

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

  def defObject: Parser[ObjectDefinition] = {
    (DefObject ~ identifier ~ optBlock(defField)) ^^ {
      case _ ~ Symbol(name) ~ fields => ObjectDefinition(name, fields)
    }
  }

  def command: Parser[CommandDefinition.type] = null
  def condition: Parser[ConditionDefinition.type] = null


  def defRule: Parser[RuleDefinition] = {
    (DefRule ~ identifier ~
      optBlock(condition) ~
      Arrow ~
      optBlock(command) ) ^^ {
      case _ ~ Symbol(name) ~ lhs ~ _ ~ rhs => RuleDefinition(name, lhs, rhs)
    }
  }



  def block: Parser[List[PreteAST]] = phrase(
    rep(trimBlock(defObject)) ~ rep(trimBlock(defRule)) ^^ {
      case objs ~ rules => objs ++ rules
    }
  )

  def program : Parser[List[PreteAST]] = block
  def apply(tokens: Seq[PreteToken]): Either[PreteParserError, List[PreteAST]] = {
    val reader = new WorkflowTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(PreteParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) => Right(result)
    }
  }
}
