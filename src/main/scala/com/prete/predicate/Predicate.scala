package com.prete.predicate

import com.prete.parser._


trait Predicate {
  def not: Predicate
}
case class PredicateClause(predicate: Predicate, args: List[Any]) extends PreteAST
case class PredicateToken(predicate: Predicate) extends PreteToken

trait PredicatesBuilder {
  def getLexers: Map[String, String => PreteToken with Predicate]
  def getPredicate: PartialFunction[Predicate, List[Any] => Boolean]
}

object PredicatesRegistry {
  private var predicates = List.empty[PredicatesBuilder]

  def addBuilder(builder: PredicatesBuilder) = {
    predicates :+= builder
  }

  def getLexers: Map[String, String => PredicateToken] = predicates
    .flatMap( _.getLexers )
    .map{ x => (x._1, (s: String) => PredicateToken(x._2(s))) }
    .toMap

  def getPredicate(predicate: Predicate) = {
    val l = predicates.map( _.getPredicate )
    l.tail.foldLeft(l.head)( _ orElse _ )(predicate)
  }

}

trait PredicatesParser extends BasicParser with BlockParser {
  def predicateName: Parser[PredicateToken] = {
    accept("Predicate token", { case p @ PredicateToken(_) => p })
  }
  def predicate: Parser[PredicateClause] = predicateName ~ opt(arguments) ^^ {
    case PredicateToken(predicate) ~ Some(args)  => {
      // TODO: Check it!!
      print(args)
      PredicateClause(predicate, args)
    }
    case PredicateToken(predicate) ~ None => PredicateClause(predicate, List.empty)
  }
}
