package recfun

object Main {
  def main(args: Array[String]) {
    /*
    println("Pascal's Triangle")
    //println(pascal(1, 3))

    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }


    println(balance("(if (z?x)mx(/1x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(balance("((h)t)u)".toList))
    println(balance("none".toList))
    */

  println(">>> Combinations=" + countChange(4, List[Int](1,2)))

  }
   /**
    * Exercise 1
    */
    def pascal(colToFind: Int, rowToFind: Int): Int = {

      def doRow(totalRows: Int, currentRow: Int, rows: Array[Array[Int]]): Array[Array[Int]] = {

        def doCol(totalCols: Int, currentCol: Int, cols: Array[Int], rows: Array[Array[Int]] ): Array[Int] = {
          if (totalCols < currentCol) {
            cols
          } else {
            if ((currentCol == 0) || (currentCol == totalCols)) {
              cols(currentCol) = 1
            } else {
              cols(currentCol) = rows(totalCols-1)(currentCol) + rows(totalCols-1)(currentCol-1)
            }

            doCol(totalCols, currentCol+1, cols, rows)
          }
        }

        if (totalRows < currentRow) {
          rows
        } else {
          rows(currentRow) = doCol(currentRow, 0, Array.range(0, currentRow+1), rows)
          doRow(totalRows, currentRow + 1, rows)
        }
      }

      val rows: Array[Array[Int]] = doRow(rowToFind, 0, Array.ofDim[Int](rowToFind+1, rowToFind+1))
      rows(rowToFind)(colToFind)
    }
  
  /**
   * Exercise 2
   *
   */
    def balance(chars: List[Char]): Boolean = {
      def findParenMatch(charsToCheck: List[Char], isBalanced: Boolean, pairCounter: Int): Boolean = {
        if (charsToCheck.isEmpty) {
          isBalanced && (pairCounter == 0)
        } else {
          if (charsToCheck.head == '(') {
            findParenMatch(charsToCheck.tail, false, pairCounter+1)
          } else if (charsToCheck.head == ')') {
            if (pairCounter > 0) {
              findParenMatch(charsToCheck.tail, true, pairCounter-1)
            } else {
              findParenMatch(charsToCheck.tail, false, pairCounter)
            }
          } else {
            findParenMatch(charsToCheck.tail, isBalanced, pairCounter)
          }
        }
      }


      findParenMatch(chars, true, 0);
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def findChangeCombos1(money: Int, coins: List[Int], i: Int): Int = {
        println(f"money=$money%2d, i=$i")
        if (money < 0)
          0
        else if (money == 0)
          1
        else if (i == coins.length && money > 0)
          0
        else
          findChangeCombos1(money - coins(i), coins, i) + findChangeCombos1(money, coins, i + 1)
      }

      def findChangeCombos2(money: Int, coins: List[Int]): Int = {
        println(f"money=$money%2d, coins=$coins")
        if ((money < 0) || coins.isEmpty)
          0
        else if (money == 0)
          1
        else {
          val newMoney = money - coins.head
          val newCoins = coins.tail
          println(s">>> call1=$newMoney, $coins; call2=$money, $newCoins")

          findChangeCombos2(newMoney, coins) + findChangeCombos2(money, newCoins)
        }
      }

      findChangeCombos2(money, coins)
    }
  }
