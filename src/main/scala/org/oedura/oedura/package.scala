package org.oedura

import scala.util.matching.Regex


package object oedura {
  /** Symbol -- terminals and non-terminals */
  sealed trait Symbol {
    val identifier: String
  }
  case class Terminal(identifier: String) extends Symbol
  case class NonTerminal(identifier: String) extends Symbol {
    def ::=(tail: List[Symbol]) = Production(this, tail)
  }

  case class Production(head: NonTerminal, tail: List[Symbol])

  case class TokenizationException(msg: String) extends Exception
  case class InvalidShiftException(msg: String) extends  Exception

  /** Instances of symbols */
  sealed trait SymbolInstance[T] {
    def symbol: Symbol
    def payload: T
  }
  case class TerminalInstance[T](symbol: Terminal, payload: T)
    extends SymbolInstance[T]
  case class NonTerminalInstance[T](symbol: NonTerminal, payload: T)

  /** Lexer items */
  sealed trait Lex {
    val pattern: String
    val regex: Regex = pattern.r
    def isWhitespace: Boolean
  }

  case class TokenLex(pattern: String, identifier: String) extends Lex {
    def isWhitespace = false
  }

  case class WhiteSpaceLex(pattern: String) extends Lex {
    def isWhitespace = true
  }

  implicit class LexBuilder(lhs: String) {
    def ~>(rhs: String): Lex = {
      if (rhs.isEmpty) WhiteSpaceLex(lhs)
      else TokenLex(lhs, rhs)
    }
  }

  /** Token */
  case class Token(value: String, lex: Lex) {
    def length: Int = value.length

    def isWhitespace: Boolean = lex.isWhitespace

    override def toString: String = lex match {
      case TokenLex(_, ident) => s"$ident($value)"
      case WhiteSpaceLex(_) => "."
    }
  }

  /** Item */
  case class Item(lhs: NonTerminal, preDot: List[Symbol], postDot: List[Symbol]) {
    override def toString: String = {
      val preDotString = preDot.map(_.identifier).mkString(" ")
      val postDotString = postDot.map(_.identifier).mkString(" ")
      lhs.identifier + " -> " + preDotString + " . " + postDotString
    }

    def advanced: Item = {
      if (!canAdvance)
        throw new InvalidShiftException("no more symbols to shift in " + this.toString)
      Item(lhs, preDot :+ postDot.head, postDot.tail)
    }

    def canAdvance: Boolean = postDot.nonEmpty

    def nextNonTerminal: Option[NonTerminal] = {
      postDot.headOption.flatMap {
        case nt: NonTerminal => Some(nt)
        case t: Terminal => None
      }
    }

    def nextTerminal: Option[Terminal] = postDot.headOption.flatMap {
      case nt: NonTerminal => None
      case t: Terminal => Some(t)
    }

    def nextSymbol: Option[Symbol] = postDot.headOption
  }

  object Item {
    def apply(p: Production): Item = {
      Item(p.head, List.empty, p.tail)
    }
  }

  implicit class ArrayOptions[T](a: Array[T]) {
    def get(i: Int): Option[T] = scala.util.Try(a(i)).toOption
    def getIndex(t: T): Option[Int] = {
      val idx = a.indexOf(t)
      if (idx == -1) None else Some(idx)
    }
  }

  /** Action -- used in parse table */
  sealed trait Action
  case class Shift(state: Int) extends Action
  case class Reduce(rule: Int) extends Action
  case object Accept extends Action
  case object ParseError extends Action
}
