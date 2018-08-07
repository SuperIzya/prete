package com.prete.core.rule

import com.prete.core.fact.Fact
import com.prete.parser.{PreteAST, PreteCompilationError, PreteParserError, Value}
import com.prete.Context

trait Condition
sealed trait AlphaCondition extends Condition
sealed trait BetaCondition extends Condition

object FieldAddress {
  type FieldAddress = (Int, String)
}
import FieldAddress._
case class Declaration(variable: String, fact: Fact) extends AlphaCondition
case class ConstantComparison(field: FieldAddress,
                              const: Value,
                              compare: (_, _) => Boolean) extends AlphaCondition

case class FieldsComparison(left: FieldAddress,
                           right: FieldAddress,
                           compare: (_, _) => Boolean) extends BetaCondition

case class Rule(name: String,
                conditions: List[Condition] = List.empty,
                command: Option[_ => _] = None)
case class RuleParseError(msg: String) extends Throwable with PreteCompilationError


object Rule {

  def apply(definition: RuleDefinition, context: Context): Rule = {
//    val conditionsAST = definition.lhs
//    val declarations = conditionsAST.collect {
//      case VarDeclaration(name, typeName) => Declaration(name, context.objects(typeName))
//    }.foldLeft(Map.empty[String, (Int, Declaration)]){
//      (acc, dec) => acc.get(dec.variable) match {
//        case None => acc + (dec.variable -> (acc.size, dec))
//        case Some(d) => throw RuleParseError(
//          s"""
//             |Variable ${dec.variable} declared more than once in rule ${definition.name}
//             |""".stripMargin)
//      }
//    }
//
//
//    val conditions = declarations.values.map( _._2 ) /*++
//      conditionsAST.map{
//        case ConstCompare(predicate, left, right) =>
//
//          ConstantComparison(
//
//        )
//      }*/
//
//    new Rule(definition.name, conditions.toList)
    null
  }

  def structure(context: Context): PartialFunction[PreteAST, Rule] = {
    case rd: RuleDefinition => Rule(rd, context)
  }


}