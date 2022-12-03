package aoc2021.day10

import scalax.collection.*
import scalax.collection.GraphEdge.*
import scalax.collection.GraphPredef.*
import scalax.collection.GraphTraversal.Visitor

object model {

  type Input = Syntax

  trait Syntax

  case class Invalid(found: Char) extends Syntax
  case object Valid extends Syntax
  case class Incomplete(stack: List[Char]) extends Syntax:
    def closeSequence:List[Char] = stack.map{
        case '(' => ')'
        case '[' => ']'
        case '{' => '}'
        case '<' => '>'
    }


      /*
      ): 3 points.
      ]: 57 points.
      }: 1197 points.
      >: 25137 points.*/



  object Syntax:
    def isOpen(c: Char): Boolean = c == '(' || c == '[' || c == '{' || c == '<'
    def isClose(c: Char): Boolean = c == ')' || c == ']' || c == '}' || c == '>'
    def isPair(open: Char, close: Char): Boolean = (open, close) match
      case ('(', ')') => true
      case ('[', ']') => true
      case ('{', '}') => true
      case ('<', '>') => true
      case _ => false


    def parseChars(seq: Seq[Char]): Syntax  =
      def loop(stack: List[Char], chars: Seq[Char]): Syntax =
        (stack, chars) match
          case (Nil, Nil) =>
            Valid  // all done
          case (Nil, c :: _) if isClose(c) =>
            Invalid(c) // close without open
          case (stack, Nil) =>
            Incomplete(stack) // done without close
          case (stack, c :: tail) if isOpen(c) =>
            loop(c :: stack, tail) // open
          case (o :: stack, c :: tail) if isClose(c) && isPair(o, c)  =>
            loop(stack, tail) // close with matching open
          case (o :: _, c :: _) if isClose(c) && !isPair(o, c) =>
            Invalid(c) // close with non-matching open
          case _ =>
            println(s"unexpected: $stack, $chars for ${seq.mkString("")}"); Invalid('x')

      loop(Nil, seq)



}
