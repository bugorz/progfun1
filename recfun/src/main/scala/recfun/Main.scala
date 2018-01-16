package recfun

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
      if (c == 0 && r == 0) 1
      else if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val parentheses = chars.filter(c => c == '(' || c == ')')
      isBalance(parentheses, 0)
    }

    private def isBalance(chars: List[Char], openParentheses: Int): Boolean = {
      if (chars.isEmpty) openParentheses == 0
      else {
        chars.head match {
          case '(' => isBalance(chars.tail, openParentheses + 1)
          case ')' =>
            if (openParentheses == 0) false
            else isBalance(chars.tail, openParentheses - 1)
          case _ => false
        }
      }
    }
  
  /**
   * Exercise 3
   */

    var sum = 0
    def countChange(money: Int, coins: List[Int]): Int = {
      sum = 0
      val sortedCoins = coins.sortWith(_ > _)
      countChanges(money, sortedCoins)
      sum
    }

    private def countChanges(money: Int, coins: List[Int]): Unit = {

      if (money == 0) {
        sum += 1
        return
      }

      if (!coins.exists(_ <= money)) {
        return
      }

      val validCoins = coins.filter(_ <= money)

      validCoins.foreach(coin => {
        countChanges(money - coin, coins.filter(_ <= coin))
      })

    }
  }
