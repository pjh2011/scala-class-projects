package recfun

import recfun.Main.countChange

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if ((c == -1) || (c > r)) {
        0
      } else if (c == 0 && r == 0) {
        1
      } else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def get_parens = (x: Char) => (x == '(' || x == ')')

      def check_sublist(chars: List[Char], n_open: Int): Boolean = {
        if (chars.isEmpty) {
          if (n_open == 0) true else false
        } else if (chars.head == '(') {
          check_sublist(chars.drop(1), n_open + 1)
        } else if (chars.head == ')') {
          if (n_open > 0) {
            check_sublist(chars.drop(1), n_open - 1)
          } else {
            false
          }
        } else false
      }

      check_sublist(chars.filter(get_parens), 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) return 1
      else if (money < 0) return 0
      else {
        coins.map(x => countChange(money - x, coins.filter(p => p <= x))).sum
      }
    }
  }
