package org.oedura.oedura

import scala.language.reflectiveCalls
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers with Parser {
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

  "Item" should "produce a string" in {
    val item = Item(E, E :: Terminal("+") :: Nil, T :: Nil)
    item.toString should be ("E -> E + . T")
  }

  it should "advance to next reader state" in {
    val item = Item(E, E :: Nil, Terminal("+") :: T ::  Nil)
    val nextItem = Item(E, E :: Terminal("+") :: Nil, T :: Nil)
    item.advanced should be (nextItem)
  }

  it should "indicate whether it can advance" in {
    val item = Item(E, E :: Terminal("+") :: Nil, T :: Nil)
    assert(item.canAdvance)
    assert(!item.advanced.canAdvance)
  }

  it should "initialize from a Production" in {
    val production = E ::= E :: Terminal("+") :: T :: Nil
    val item = Item(production)
    item should be (Item(E, List.empty, E :: Terminal("+") :: T :: Nil))
  }

  it should "produce next NonTerminal" in {
    val item = Item(rules(1))
    // E -> . E + T
    item.nextNonTerminal should be (Some(E))
    // E => E . + T (terminal next)
    item.advanced.nextNonTerminal should be (None)
    val emptyItem = Item(E, List(T), List.empty)
    // E -> T . (no elements left)
    emptyItem.nextNonTerminal should be (None)
  }

  it should "produce next Terminal" in {
    val item = Item(S, E :: Nil, Terminal("+") :: T :: Nil)
    item.nextTerminal should be (Some(Terminal("+")))
    item.advanced.nextTerminal should be (None) // read T
    item.advanced.advanced.nextTerminal should be (None) // read end of rule
  }

  it should "produce next Symbol" in {
    val item = Item(rules(1))
    // E -> . E + T
    item.nextNonTerminal should be (Some(E))
    // E => E . + T (terminal next)
    item.advanced.nextNonTerminal should be (Some(Terminal("+")))
    val emptyItem = Item(E, List(T), List.empty)
    // E -> T . (no elements left)
    emptyItem.nextNonTerminal should be (None)
  }

  "ItemSet" should "identify shift symbols" in {
    ItemSet(Item(rules.head) :: Nil).shiftSymbols.toSet should be {
      Set(Terminal("id"), E, T)
    }
  }

  "Closure" should "generate a closure" in {
    val recItem = Item(S, Nil, E :: Nil)
    val recItemClosure = Set(
      Item(S, Nil, E :: Nil),
      Item(E, Nil, E :: Terminal("+") :: T :: Nil),
      Item(E, Nil, T :: Nil),
      Item(T, Nil, Terminal("id") :: Nil)
    )
    closure(List(recItem)).length should be (4)
    closure(List(recItem)).toSet should be (recItemClosure)

    val termItem = Item(E, E :: Nil, Terminal("+") :: T :: Nil)
    val termItemClosure = List(termItem)
    closure(List(termItem)) should be (termItemClosure)

    val advItem = Item(E, E :: Terminal("+") :: Nil, T :: Nil)
    val advItemClosure = Set(
      Item(E, E :: Terminal("+") :: Nil, T :: Nil),
      Item(T, Nil, Terminal("id") :: Nil)
    )
    closure(List(advItem)).length should be (2)
    closure(List(advItem)).toSet should be (advItemClosure)
  }


  "Parser" should "find its non-terminals" in {
    this.nonTerminals.toSet should be (Set(S, E, T))
  }

}
