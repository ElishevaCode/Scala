package recfun

import recfun.RecFun.countChange

object RecFun extends RecFunInterface :

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      val rowsString = (0 to row).toList
        .map(col => s"${pascal(col, row)} ")
        .reduce((s1, s2) => s1 + s2)
      print(rowsString)
      println()

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def score(chars: List[Char], acc: Int = 0): Int =
      if (chars.isEmpty || acc < 0) acc
      else {
        val c = chars.head
        score(chars.tail, if (c == '(') acc + 1 else if (c == ')') acc - 1 else acc)
      }

    score(chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty && money >= 1) 0
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

