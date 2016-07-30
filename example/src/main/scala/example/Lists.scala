package example


object Lists {

  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
    //def sum(xs: List[Int]): Int = xs.foldLeft(0)((m: Int, n: Int) => m + n)

    def sum(xs: List[Int]): Int = {
      def loop(acc: Int, xs: List[Int]): Int =
        if (xs.isEmpty) acc else loop(acc + xs.head, xs.tail)

      loop(0, xs)
    }
  
  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
    def max(xs: List[Int]): Int = {
      def loop(max: Int, xs: List[Int]): Int =
        if (xs.isEmpty) {
          max
        } else {
          if (xs.head > max)
            loop(xs.head, xs.tail)
          else
            loop(max, xs.tail)
        }

      if (xs.isEmpty)
        throw new java.util.NoSuchElementException("no max for empty list")
      else
        loop(0, xs)
    }
  }
