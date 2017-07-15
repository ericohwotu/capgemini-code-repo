package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    val ls1 = List('(','i',')','(','n',')')
    val ls2 = List('(','i','f','(','Z','e','r','o','?','x',')','m','a','x','(','/','1','x',')',')')
    val ls3 = List('(',')',')','(')
    val ls4 = "I told him (that it’s not (yet) done). (But he wasn’t listening)".toList
    val ls5 = "this has no quotes"
    println(s"1 is balanced ${balance(ls1)}")
    println(s"2 is balanced ${balance(ls2)}")
    println(s"3 is balanced ${balance(ls3)}")
    println(s"4 is balanced ${balance(ls4)}")
    println(s"$ls5 is balanced ${balance(ls5.toList)}")
    println(countChange(6, List(1, 2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = c match {
    case _ if c == 0 || c == r => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    var balanced = true; var bCount = 0
    @tailrec
    def looper(chrs: List[Char]): Boolean = {
      chrs match {
        case x if x.isEmpty => if(bCount%2==0&&balanced)true else false
        case x if x.head == '(' && balanced || x.head == ')' && !balanced => balanced = !balanced; bCount+=1; looper(chrs.tail)
        case x if x.head == '(' && !balanced || x.head == ')' && balanced => bCount+=1;looper(chrs.tail)
        case _ => looper(chrs.tail)
      }
    }
    looper(chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = money match {
    case _ if money == 0 => 1
    case _ if coins.isEmpty || money < 0 => 0
    case _ => countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}
