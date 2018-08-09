package com.prete.core.predicate

import com.prete.core.network.Blueprint
import com.prete.core.rule.{AlphaCondition, Condition}
import com.prete.core.{BuildersRegistry, OperationContainer, PreteBuilder}
import com.prete.parser._


trait Predicate extends Condition
case class PredicateClause(predicate: Predicate, args: List[Any]) extends PreteAST
case class PredicateToken(predicate: Predicate) extends PreteToken with OperationContainer
case class PredicateCompilationError(predicate: String, msg: String) extends PreteCompilationError
case class VarDeclaration(name: String, typeName: String) extends PreteAST with AlphaCondition

case class PredicateBlueprint(predicate: Predicate, args: List[Blueprint]) extends Blueprint


trait PredicatesBuilder
  extends PreteBuilder[PreteToken with Predicate, PredicateClause] {

  def getPredicate: PartialFunction[Predicate, List[Any] => Boolean]

  def token(data: Predicate) = PredicateToken(data)

  def blueprint(clause: PredicateClause)
               (implicit varContext: List[VarDeclaration]) = {
    case PredicateClause(p, args) => null // TODO !!!
  }
}



class PredicatesRegistry extends BuildersRegistry[PredicatesBuilder] {

  def getPredicate(predicate: Predicate) = {
    val l = builders.map( _.getPredicate )
    l.tail.foldLeft(l.head)( _ orElse _ )(predicate)
  }

}

object PredicatesRegistry {
  def apply(): PredicatesRegistry = new PredicatesRegistry()
}
