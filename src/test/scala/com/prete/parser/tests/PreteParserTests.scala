package com.prete.parser.tests
import com.prete.Environment
import com.prete.core.fact.{FactDefinition, FieldDefinition}
import org.scalatest._

class PreteParserTests extends FlatSpec with Matchers with EitherValues with Inspectors with Inside {
  "PreteParser" should "parse proper object definition" in {

    val code = """
        |fact Fact1
        |
        |fact Fact2
        | field1
        |
      """.stripMargin

    val env = Environment()
    val result = env.parse(code)

    result should be('right)
    val lst: List[Any] = result.right.value.asInstanceOf[List[_]]
    (lst.head, lst(1)) match {
      case (FactDefinition(name1, fields1), FactDefinition(name2, fields2)) => {
        name1 should be ("Fact1")
        name2 should be ("Fact2")
        fields1 should be ('empty)
        fields2 should not be ('empty)
        inside(fields2.head) {
          case FieldDefinition(name) => name should be ("field1")
        }
      }
    }

  }

  "PreteParser" should "fail on wrong input" in {
    val env = Environment()
    val results = List(
      env.parse("asd878.9989saf"),
      env.parse("34as")
    )

    forAll(results) { r =>
      r should be ('left)

    }
  }
}
