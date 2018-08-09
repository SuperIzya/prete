package com.prete.core.rule

import com.prete.parser.{PreteAST, PreteCompilationError, Value}
import com.prete.Context
import com.prete.core.network.Blueprint
import com.prete.core.predicate.{Predicate, PredicateClause, VarDeclaration}

trait Condition extends Blueprint
trait AlphaCondition extends Condition
trait BetaCondition extends Condition
case class Rule(name: String,
                conditions: List[Condition] = List.empty,
                command: Option[_ => _] = None)
case class RuleParseError(msg: String) extends Throwable with PreteCompilationError

case class RuleCompilationError(rule: String, msg: String) extends PreteCompilationError

object Rule {
  type FieldAddress = (Int, String)

  case class ConstantComparison(field: FieldAddress,
                                const: Value,
                                predicate: Predicate) extends AlphaCondition

  case class FieldsComparison(left: FieldAddress,
                              right: FieldAddress,
                              predicate: Predicate) extends BetaCondition




  def apply(definition: RuleDefinition)
           (implicit context: Context): Either[RuleCompilationError, Rule] = {
    if (context.ruleDefinitions.contains(definition.name))
      return Left(
        RuleCompilationError(definition.name, s"Rule ${definition.name} already defined")
      )

    val lhs = definition.lhs

    var varContext = List.empty[VarDeclaration]
    def buildCondition: PartialFunction[PreteAST, Either[RuleCompilationError, Condition]] = {
      case d @ VarDeclaration(name, typeName) => {
        if (varContext.exists {
          case VarDeclaration(n, _) => n == name
        }) {
          Left(
            RuleCompilationError(definition.name, s"Duplicate declaration of variable $name")
          )
        } else {
          varContext :+= d
          Right(d)
        }
      }
      case clause @ PredicateClause(p, args) =>

    }

    val conditions = lhs.map { buildCondition(_) }


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

  def compiler(implicit context: Context): PartialFunction[PreteAST, Either[RuleCompilationError, Rule]] = {
    case rd: RuleDefinition => Rule(rd)
  }



}