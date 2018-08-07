package com.prete.parser.tests
import org.scalatest._
import com.prete.parser.{PreteTokenizer, Tokens, WithValue}

class PreteTokenizerTests extends FlatSpec with Matchers with EitherValues with Inspectors with Inside {

  "PreteTokenizer" should "parse primitive values correctly" in {
    val tokenizer = PreteTokenizer()

    val data = List(1, -1, +1, "\"abx\"", "asdsad22344_234sdf32", "\"a234sadf asdasd 32\"", 1.1f, -32.23f)
    val results1 = data.map{ v => (v, tokenizer(s"$v"))}
    forAll(results1) { r =>
      r._2 should be ('right)
      r._2.right.value shouldNot be(empty)
      val res = r._2.right.value.head.asInstanceOf[WithValue[_]]
      res match {
        case _: Tokens.String =>
          val str = r._1.asInstanceOf[String]
          val v = str.substring(1, str.length - 1)
          res.value should be(v)
        case _ => res.value should be(r._1)
      }
    }
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
