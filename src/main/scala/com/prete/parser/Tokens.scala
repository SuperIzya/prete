package com.prete.parser


object Tokens {
  type _Integer = java.lang.Integer
  type _Float = java.lang.Float

  case class Variable(name: String) extends PreteAST
  case class Symbol(value: String) extends PreteAST with WithValue[String]
  case class Integer(value: Int) extends PreteAST with WithValue[Int]
  case class Float(value: _Float) extends PreteAST with WithValue[_Float]
  case class Text(value: String) extends PreteAST with WithValue[String]

  case object Colon extends PreteAST

  case class IndentCount(length: Int) extends PreteAST
  case object Dedent extends PreteAST
  case object Indent extends PreteAST
  case object Arrow extends PreteAST
  case object BackArrow extends PreteAST
  case object Dot extends PreteAST

  case object Comma extends PreteAST
  case object OpenBr extends PreteAST
  case object CloseBr extends PreteAST

  case object DefFact extends PreteAST
  case object DefRule extends PreteAST
  case object Whitespace extends PreteAST
}
