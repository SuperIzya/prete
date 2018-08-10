package com.prete.core

import com.prete.parser.PreteAST
import com.prete.types.PreteType

package object fact {

  case class Field(name: String, preteType: PreteType)

  case class Fact(name: String, fields: Map[String, Field] = Map.empty)

  case class FactInstance(name: String, fields: Map[String, Any] = Map.empty)

  case class FieldDefinition(name: String) extends PreteAST
  case class FactDefinition(name: String,
                            fields: List[FieldDefinition]) extends PreteAST
}
