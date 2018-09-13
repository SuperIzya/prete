package com.prete.core.predicate

import com.prete.core.fact.FactParser
import com.prete.parser.{BasicParser, BlockParser, PreteAST}
import com.prete.parser.Tokens.{BackArrow, Dedent, Indent, Symbol}

trait PredicatesParser extends BasicParser with BlockParser with FactParser {
  def predicateName: Parser[PredicateAST] = {
    accept("Predicate token", { case p @ PredicateAST(_) => p })
  }

  def predicateArgs: Parser[PredicateClause] = predicateName ~ opt(arguments) ^^ {
    case PredicateAST(predicate) ~ Some(args)  => {
      // TODO: Check it!!
      print(args)
      PredicateClause(predicate, args)
    }
    case PredicateAST(predicate) ~ None => PredicateClause(predicate, List.empty)
  }

  def predicateExpr: Parser[PredicateClause] =
    predicateName ~
      Indent ~
      rep1(clause) ~
      Dedent ^^ {
      case PredicateAST(predicate) ~ _ ~ args ~ _ => PredicateClause(predicate, args)
    }

  def predicate: Parser[PredicateClause] = predicateArgs | predicateExpr

  def declaration: Parser[VarDeclaration] = identifier ~ BackArrow ~ identifier ^^ {
    case Symbol(name) ~ _ ~ Symbol(factName) => VarDeclaration(name, factName)
  }
  def clause: Parser[PreteAST] = (declaration | predicate) ^^ { x => x }
}
