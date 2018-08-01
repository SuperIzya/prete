package com.prete
import com.prete.constructs.{Object, Rule}
import com.prete.parser.PreteAST

trait PreteStructure
trait Builder {
  def build(context: Context): PartialFunction[PreteAST, PreteStructure]
}

case class Context(objects: Map[String, Object] = Map.empty,
                   rules: Map[String, Rule] = Map.empty)
