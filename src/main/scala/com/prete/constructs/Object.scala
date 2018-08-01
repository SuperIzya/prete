package com.prete.constructs

import com.prete.types.PreteType

case class Field(name: String, preteType: PreteType)
case class Object(name: String, fields: Map[String, Field] = Map.empty)

