package org.oedura.oedura

import scala.language.reflectiveCalls
import org.scalatest._

class LexerSpec extends FlatSpec with Matchers {

  def fixture = new {
    val num = "\\d+" ~> "num"
    val word = "[a-zA-Z_]+" ~> "word"
    val ws = "\\s+" ~> ""

    val tlex = new Lexer {
      val lex = List(num, word, ws)
    }

    val tokenList = List(
      Token("1", num),
      Token("2", num),
      Token("foo", word),
      Token("bar", word),
      Token("three", word),
      Token("4", num),
      Token("five", word)
    )
  }

  "Lexer" should "construct Lexers from IDL" in {
    ("\\s+" ~> "") shouldBe a [WhiteSpaceLex]
    ("\\w+" ~> "word") should equal (TokenLex("\\w+", "word"))
  }

  /*
  it should "produce terminals from lex list" in {
    val tlex_empty = new Lexer {
      val lex = List.empty
    }
    tlex_empty.terminals should equal (List.empty[Symbol])

    val tlex_no_ws = new Lexer {
      val lex = List(
        "\\d+" ~> "number",
        "[A-Za-z]\\w*" ~> "word"
      )
    }
    tlex_no_ws.terminals should equal (Terminal("number") :: Terminal("word") :: Nil)

    val tlex_ws_only = new Lexer {
      val lex = List(
        "\\s+" ~> ""
      )
    }
    tlex_ws_only.terminals should equal (List.empty)

    val tlex_mix = new Lexer {
      val lex = List(
        "\\d+" ~> "number",
        "[A-Za-z]\\w*" ~> "word",
        "\\s+" ~> ""
      )
    }
    tlex_mix.terminals should equal (Terminal("number") :: Terminal("word") :: Nil)
  }
  */


  it should "tokenize a string" in {
    val f = fixture

    val tokens = f.tlex.tokenize("1 2 foo bar three4five")
    tokens should equal (f.tokenList)
  }

  "Token" should "produce formated string output" in {
    val lex = "\\w+" ~> "word"
    val token = Token("foo", lex)
    token.toString should equal ("word(foo)")
  }
}
