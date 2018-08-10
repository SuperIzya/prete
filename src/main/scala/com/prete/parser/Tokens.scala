package com.prete.parser


object Tokens {

  type _String = java.lang.String
  type _Integer = java.lang.Integer
  type _Float = java.lang.Float

  case class Symbol(value: _String) extends PreteToken with WithValue[_String]
  case class Integer(value: Int) extends PreteToken with WithValue[Int]
  case class Float(value: _Float) extends PreteToken with WithValue[_Float]
  case class String(value: _String) extends PreteToken with WithValue[_String]

  case object Colon extends PreteToken

  case class IndentCount(length: Int) extends PreteToken
  case object Dedent extends PreteToken
  case object Indent extends PreteToken
  case object Arrow extends PreteToken
  case object BackArrow extends PreteToken
  case object Dot extends PreteToken

  case object Comma extends PreteToken
  case object OpenBr extends PreteToken
  case object CloseBr extends PreteToken

  case object DefFact extends PreteToken
  case object DefRule extends PreteToken
  case object Whitespace extends PreteToken
}
