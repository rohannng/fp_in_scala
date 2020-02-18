package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   *
   * 1
   * 1  1
   * 1  2 1
   * 1  3 3 1
   * 1  4 6 4 1
   *
   */
  def pascal(c: Int, r: Int): Int = {

    if (c == 0 || r == 0 || c == r) 1
    else if (c > r) 0
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceAccu(accu: Int, rest: List[Char]): Boolean = {
      if (rest.isEmpty) {
        accu == 0
      }
      else if (rest.head == '(') balanceAccu(accu + 1, rest.tail)
      else if (rest.head == ')') {
        if (accu > 0) balanceAccu(accu - 1, rest.tail) else false
      } else balanceAccu(accu, rest.tail)
    }

    balanceAccu(0, chars)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {


    def countIter(ways: Int, money: Int, coins: List[Int]): Int = {
      if (money == 0) ways
      else if (coins.length == 0) ways
      else {
        if (money - coins.head == 0) ways + 1
        else if (money - coins.head < 0) ways
        else countIter(ways, money - coins.head, coins) +
          countIter(ways, money, coins.tail)
      }
    }

    countIter(0, money, coins.sorted)
  }

}
