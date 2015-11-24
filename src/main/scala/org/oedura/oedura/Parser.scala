package org.oedura.oedura

import scala.annotation.tailrec


trait Parser {
  val rules: List[Production]

  lazy val nonTerminals: List[NonTerminal] = rules.map(_.head).distinct

  def closure(start: List[Item]): List[Item] = {
    def closureImpl(
        items: List[Item],
        stack: List[NonTerminal],
        done: List[NonTerminal]): List[Item] =
    {
      if (stack.isEmpty) items else {
        val newItems = rules
          .filter(_.head == stack.head)
          .map(Item(_))
          .filterNot(items contains _)

        val newNonTerminals = newItems
          .flatMap(_.nextNonTerminal)
          .filterNot(done contains _)
          .distinct

        closureImpl(
          items ++ newItems,
          newNonTerminals ++ stack.tail,
          stack.head :: done
        )
      }
    }

    val startingNonTerminals = start.flatMap(_.nextNonTerminal).distinct
    closureImpl(start, startingNonTerminals, List.empty)
  }

  case class ItemSet(seed: List[Item]) {
    lazy val full: List[Item] = closure(seed)

    def shiftNonTerminals: List[NonTerminal] = full
      .flatMap(_.nextNonTerminal)
      .distinct

    def shiftTerminals: List[Terminal] = full
      .flatMap(_.nextTerminal)
      .distinct

    def shiftSymbols: List[Symbol] = shiftTerminals ++ shiftNonTerminals

    def reduceItems: List[Item] = full
      .filter(r => r.preDot.nonEmpty && r.postDot.isEmpty)
  }

  type ActionTable = Map[(Int, Symbol), Action]
  type GotoTable = Map[(Int, NonTerminal), Int]

  final case class ParserBuilder(
      dfa: Array[ItemSet],
      actions: ActionTable,
      goto: GotoTable,
      todo: List[ItemSet]) {

    // Either an existing state or a new one
    def transitionFromToken(sym: Symbol, itemSet: ItemSet): Either[Int, ItemSet] = {
      val nextSetItems = itemSet.full
        .filter(_.nextSymbol == sym)
        .filter(_.canAdvance)
        .map(_.advanced)

      val nextSet = ItemSet(nextSetItems)

      dfa.getIndex(nextSet).toLeft(nextSet)
    }

    def popTodo: ParserBuilder = this.copy(todo = todo.tail)

    def appendNext: ParserBuilder = {
      val nextIS = todo.head
      val nextIndex = dfa.length + 1

      val nextDfa = dfa :+ nextIS



      ???
    }

    def step: ParserBuilder = {
      if (dfa contains todo.head) this.popTodo
      else this.appendNext
    }

    @tailrec
    def build: ParserBuilder = {
      if (todo.isEmpty) this
      else this.step.build
    }
  }
}

class TestParser extends Parser {
  val id = Terminal("id")
  val plus = Terminal("+")
  val $ = Terminal("$")

  val S = NonTerminal("S")
  val E = NonTerminal("E")
  val T = NonTerminal("T")

  val rules: List[Production] = List(
    S ::= E :: Nil,
    E ::= E :: Terminal("+") :: T :: Nil,
    E ::= T :: Nil,
    T ::= Terminal("id") :: Nil
  )
}
