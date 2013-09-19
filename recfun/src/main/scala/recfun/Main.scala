package recfun
import common._
import scala.collection.immutable.Stack
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    println("string balance")
    def stringbalancetester(strings: ArrayBuffer[String], balanced: Boolean) {
      strings.map(str => {
        print("testing string" + str)
        if (balance(str.toList) == balanced) println(" : works!")
        else println(" : failed")
      })
    }     
    stringbalancetester(ArrayBuffer("(just an) example", "(if (zero? x) max (/ 1 x))", "I told him (that it’s not (yet) done). (But he wasn’t listening)"), true)
    stringbalancetester(ArrayBuffer(":-)", "())("), false)
    
  }

  /**
   * Exercise 1
   *
   * The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of
   * the two numbers above it. Write a function that computes the elements of Pascal’s triangle by means
   * of a recursive process.
   * Do this exercise by implementing the pascal function in Main.scala, which takes a column c and a row r
   * ,counting from 0 and returns the number at that spot in the triangle. For example, pascal(0,2)=1,
   * pascal(1,2)=2 and pascal(1,3)=3.
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   * Write a recursive function which verifies the balancing of parentheses in a string,
   * which we represent as a List[Char] not a String.
   */
  def balance(chars: List[Char]): Boolean = {

    def loop(stack: Stack[Char], str: List[Char]): Boolean = {
      if (str.isEmpty) stack.length == 0
      else {
        str.head match {
          case '(' => loop(stack.push(str.head), str.tail)
          case ')' if stack.length == 0 => false
          case ')' => loop(stack.pop, str.tail)
          case _ => loop(stack, str.tail)
        }
      }
    }

    loop(new Stack[Char], chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
