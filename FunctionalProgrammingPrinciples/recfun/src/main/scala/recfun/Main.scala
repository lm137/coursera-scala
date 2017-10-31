package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    Seq("(if (zero? x) max (/ 1 x))", "I told him (that it’s not (yet) done). (But he wasn’t listening)", ":-)", "())(").foreach {
      str => println(s"$str: ${balance(str.toList)}")
    }
    
    println(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = { (c, r) match {
      case (0, _) => 1
      case (x, y) if x == y => 1
      case otherwise => pascal(c-1,r-1) + pascal(c,r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def tailBalance(chars: List[Char], openParens: Int) : Boolean = {
        if (openParens < 0) false
        else if (chars.isEmpty) openParens == 0
        else if (chars.head == '(') {
          tailBalance(chars.tail, openParens + 1)
        } else if (chars.head == ')') {
          tailBalance(chars.tail, openParens - 1)
        } else tailBalance(chars.tail, openParens)
      }
      
      tailBalance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0)  1
      else if (money < 0) 0
      else coins.map(c => countChange(money - c, coins.filter(_ <= c))).sum
      
    }
  }
