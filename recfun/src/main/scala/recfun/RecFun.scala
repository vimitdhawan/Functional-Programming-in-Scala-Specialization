package recfun
import scala.annotation.tailrec

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
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def checkBalance(chars: List[Char], acc:Int): Boolean = {
      chars match {
        case head :: tail => head match {
          case '(' => checkBalance(tail, acc+1)
          case ')' => if(acc == 0) false else checkBalance(tail, acc-1)
          case _ => checkBalance(tail, acc)
        }
        case _ => acc == 0
      }
    }
    checkBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def calculateCountChange(money: Int, leftCoins: List[Int], acc: Int): Int = {
      if(money<0) acc
      else if(leftCoins.isEmpty)
        if(money == 0) acc+1 else acc
      else calculateCountChange(money - leftCoins.head, leftCoins, acc) + calculateCountChange(money, leftCoins.tail, acc)
    }
    calculateCountChange(money, coins, 0)

  }
}
