package com.prete.core.predicate

import com.prete.core.{BuildersRegistry, OperationContainer, PreteBuilder}
import com.prete.core.fact.FactParser
import com.prete.parser.Tokens.{BackArrow, Dedent, Indent, Symbol}
import com.prete.parser._


trait Predicate
case class PredicateClause(predicate: Predicate, args: List[Any]) extends PreteAST
case class PredicateToken(predicate: Predicate) extends PreteToken with OperationContainer

trait PredicatesBuilder extends PreteBuilder[PreteToken with Predicate, PredicateClause] {
  def getPredicate: PartialFunction[Predicate, List[Any] => Boolean]
}
case class VarDeclaration(name: String, typeName: String) extends PreteAST

class PredicatesRegistry extends BuildersRegistry[PredicateToken,
                                                  PredicateClause,
                                                  PredicatesBuilder] {

  def getPredicate(predicate: Predicate) = {
    val l = builders.map( _.getPredicate )
    l.tail.foldLeft(l.head)( _ orElse _ )(predicate)
  }

}

object PredicatesRegistry {
  def apply(): PredicatesRegistry = new PredicatesRegistry()
}

trait PredicatesParser extends BasicParser with BlockParser with FactParser {
  def predicateName: Parser[PredicateToken] = {
    accept("Predicate token", { case p @ PredicateToken(_) => p })
  }

  def predicateArgs: Parser[PredicateClause] = predicateName ~ opt(arguments) ^^ {
    case PredicateToken(predicate) ~ Some(args)  => {
      // TODO: Check it!!
      print(args)
      PredicateClause(predicate, args)
    }
    case PredicateToken(predicate) ~ None => PredicateClause(predicate, List.empty)
  }

  def predicateExpr: Parser[PredicateClause] =
    predicateName ~
      Indent ~
      rep1(clause) ~
      Dedent ^^ {
      case PredicateToken(predicate) ~ _ ~ args ~ _ => PredicateClause(predicate, args)
    }

  def predicate: Parser[PredicateClause] = predicateArgs | predicateExpr

  def declaration: Parser[VarDeclaration] = identifier ~ BackArrow ~ identifier ^^ {
    case Symbol(name) ~ _ ~ Symbol(factName) => VarDeclaration(name, factName)
  }
  def clause: Parser[PreteAST] = (declaration | predicate) ^^ { x => x }
}
