package com.prete.parser

import scala.util.parsing.combinator.Parsers


trait BlockParser extends Parsers with PreteTokenParser {
  import Tokens._

  def optBlock[TypeAST](P: Parser[TypeAST]): Parser[List[TypeAST]] =
    opt(Indent ~> rep1(P) <~ Dedent) ^^ {
      case None => List.empty
      case Some(value) => value
    }

  def trimBlock[TypeAST](P: Parser[TypeAST]): Parser[TypeAST] =
    rep(Indent | Dedent) ~> P <~ rep(Indent | Dedent) ^^ { vals => vals }

}