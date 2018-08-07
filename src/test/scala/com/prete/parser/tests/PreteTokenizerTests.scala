package com.prete.parser.tests
import org.scalatest._


import com.prete.parser.{PreteTokenizer, Tokens}

class PreteTokenizerTests extends FlatSpec with Matchers with EitherValues with Inspectors with Inside {

  "PreteTokenizer" should "parse primitive values correctly" in {

    val intVal = 1
    val stringVal = "abc"
    val floatVal = 1.1f
    val symbolVal = "abc21313_1230"
    val objectName = "SuperObject"

    val tokenizer = PreteTokenizer()

    val intRes = tokenizer(s"$intVal")
    val stringRes = tokenizer(s""""$stringVal"""")
    val floatRes = tokenizer(s"$floatVal")
    val symbolRes = tokenizer(s"$symbolVal")
    val defObjectRes = tokenizer(s"object $objectName")

    val results = Seq(
      intRes,
      stringRes,
      floatRes,
      symbolRes,
      defObjectRes
    )

    forAll(results) { r =>
      r should be ('right)
    }

    val values = results.map {
      _.right.value
    }

    inside(values(0)) { case List(Tokens.Integer(v)) => v should be(intVal) }
    inside(values(1)) { case List(Tokens.String(v)) => v should be(stringVal) }
    inside(values(2)) { case List(Tokens.Float(v)) => v should be(floatVal) }
    inside(values(3)) { case List(Tokens.Symbol(v)) => v should be(symbolVal) }
    inside(values(4)) { case List(Tokens.DefFact, Tokens.Symbol(n)) => n should be(objectName)}
  }

  "PreteTokenizer" should "fail on wrong input" in {
    val tokenizer = PreteTokenizer()
    val results = Seq(
      tokenizer("abc98.90cba"),
      tokenizer("abc-abc")
    )

    forAll(results) { r =>
      r should be ('left)
    }
  }

}
