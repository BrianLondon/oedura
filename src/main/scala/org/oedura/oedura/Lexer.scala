package org.oedura.oedura

trait Lexer {
  val lex: List[Lex]

  def tokenize(input: String): List[Token] = {
    def tokenizeImpl(done: List[Token], remaining: String): List[Token] = {
      if (remaining.isEmpty) done else {
        val matches: List[Token] = lex flatMap { testLex =>
          testLex.regex.findPrefixOf(remaining) map { str =>
            Token(str, testLex)
          }
        }

        if (matches.isEmpty)
          throw new TokenizationException("Unexpected token: " + input.take(10) + "...")

        val charsRead = matches.head.length

        if (matches.head.isWhitespace) tokenizeImpl(done, remaining.drop(charsRead))
        else tokenizeImpl(matches.head :: done, remaining.drop(charsRead))
      }
    }

    tokenizeImpl(List.empty[Token], input).reverse
  }

}
