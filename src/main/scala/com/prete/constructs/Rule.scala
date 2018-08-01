package com.prete.constructs

import com.prete.{Builder, Context, PreteStructure, constructs}
import com.prete.parser.{ConstCompare, Declaration, RuleDefinition, Value}

trait Condition
sealed trait AlphaCondition extends Condition
sealed trait BetaCondition extends Condition

object FieldAddress {
  type FieldAddress = (Int, String)
}
import FieldAddress._
case class Declaration(variable: String, template: Object) extends AlphaCondition
case class ConstantComparison(field: FieldAddress,
                              const: Value,
                              compare: (_, _) => Boolean) extends AlphaCondition

case class FieldsComparison(left: FieldAddress,
                           right: FieldAddress,
                           compare: (_, _) => Boolean) extends BetaCondition

case class Rule(name: String,
                conditions: List[Condition] = List.empty,
                command: _ => _ = _ => _) extends PreteStructure


object Rule extends Builder {

  def apply(definition: RuleDefinition, context: Context): Rule = {
    val conditionsAST = definition.lhs
    val declarations = conditionsAST.collect {
      case Declaration(name, typeName) => constructs.Declaration(name, objects(typeName))
    }.foldLeft(Map.empty[String, (Int, Declaration)]){
      (acc, dec) => acc.get(dec.variable) match {
        case None => acc + (dec.variable -> (acc.size, dec))
      }
    }


    val conditions = declarations.values.map( _._2 ) /*++
      conditionsAST.map{
        case ConstCompare(predicate, left, right) =>

          ConstantComparison(

        )
      }*/

    new Rule(definition.name, conditions.toList)
  }

  def build(context: Context) = {
    case rd: RuleDefinition => Rule(rd, context)
  }
}