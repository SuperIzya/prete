package com.prete.core.fact

import com.prete.types.PreteType

case class Field(name: String, preteType: PreteType)
case class Fact(name: String, fields: Map[String, Field] = Map.empty)

