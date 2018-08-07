package com.prete.parser.tests
import org.scalatest._


import com.prete.parser.{PreteLexer, Tokens}

class PreteLexerTests extends FlatSpec with Matchers with EitherValues with Inspectors with Inside {

  "PreteLexer" should "parse primitive values correctly" in {

    val intVal = 1
    val stringVal = "abc"
    val floatVal = 1.1f
    val symbolVal = "abc21313_1230"
    val objectName = "SuperObject"

    val intRes = PreteLexer(s"$intVal")
    val stringRes = PreteLexer(s""""$stringVal"""")
    val floatRes = PreteLexer(s"$floatVal")
    val symbolRes = PreteLexer(s"$symbolVal")
    val defObjectRes = PreteLexer(s"object $objectName")


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

  "PreteParser" should "fail on wrong input" in {
    val results = Seq(
      PreteLexer("abc98.90cba"),
      PreteLexer("abc-abc")
    )

    forAll(results) { r =>
      r should be ('left)
    }
  }

}
