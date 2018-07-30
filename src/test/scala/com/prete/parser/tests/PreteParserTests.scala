package com.prete.parser.tests
import com.prete.parser.{FieldDefinition, ObjectDefinition, PreteCompiler}
import org.scalatest._

class PreteParserTests extends FlatSpec with Matchers with EitherValues with Inspectors with Inside {
  "PreteParser" should "parse proper object definition" in {

    val result = PreteCompiler(
      """
        |object SuperObject1
        |
        |object SuperObject2
        | field1
        |
      """.stripMargin)

    result should be('right)
    val lst: List[Any] = result.right.value.asInstanceOf[List[_]]
    (lst(0), lst(1)) match {
      case (ObjectDefinition(name1, fields1), ObjectDefinition(name2, fields2)) => {
        name1 should be ("SuperObject1")
        name2 should be ("SuperObject2")
        fields1 should be ('empty)
        fields2 should not be ('empty)
        inside(fields2.head) {
          case FieldDefinition(name) => name should be ("field1")
        }
      }
    }

  }
}
