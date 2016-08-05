package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
     val message = "hello, world"
     assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
   test("adding ints") {
     assert(1 + 2 === 3)
   }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)

    val s1234 = union(union(s1,s2), union(s3, s4))
    val lowerToZero = union(singletonSet(-FunSets.bound), singletonSet(0))
    val lowerToUpper = union(singletonSet(-FunSets.bound), singletonSet(FunSets.bound))

    val s1to7and100 = union(s1234, union(union(singletonSet(5), singletonSet(7)), singletonSet(FunSets.bound)))
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)

      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains intersecting elements of each set") {
    new TestSets {
      val us1 = union(s1, s2)
      val us2 = union(us1, s3)

      val is1 = intersect(us1, us2)

      assert(contains(is1, 1) && contains(is1, 2) && !contains(is1, 3), "Intersection contains 1 and 2 but not 3")
    }
  }


  test("diff returns elements from s not in t") {
    new TestSets {
      val us1 = union(s1, s2)
      val us2 = union(s2, s3)

      val ds1 = diff(us1, us2)

      assert(contains(ds1, 1) && !contains(ds1, 2) && !contains(ds1, 3),
        "diff " + FunSets.toString(us1) + " and " + FunSets.toString(us2) +
          " yields " + FunSets.toString(ds1) + " (should be {1})")

      val ds2 = diff(lowerToZero, s1234)
      assert(contains(ds2, -FunSets.bound) && contains(ds2, 0) && !contains(ds2, 3) && !contains(ds2, 4), "diff should not contain " + FunSets.toString(s1234) + " but does")

      val ds3 = diff(s1to7and100, s1234)
      assert(contains(ds3, 5) && contains(ds3, 7) && contains(ds3, FunSets.bound), FunSets.toString(ds3) + " did not equal {5,7,1000}")
    }
  }

  test("filter returns element based on impl of given filter function") {
    new TestSets {

      val us2 = union(union(s1, s2), s3)

      val fs1 = filter(us2, elem => elem >= 2)


      assert(!contains(fs1, 1) && contains(fs1, 2) && contains(fs1, 3), "filter elem >= 2, contains 2 and 3 but not 1")
    }
  }

  test("forall tests") {
    new TestSets {

      val us1 = union(union(s1, s2), s3)

      val allElementsMoreThanZero = forall(us1, elem => elem >= 0)
      assert(allElementsMoreThanZero, "all elements in set " + FunSets.toString(us1) + " should be more than zero")

      val allElementsLessThanZero = forall(us1, elem => elem <= 0)
      assert(!allElementsLessThanZero, "elements in set " + FunSets.toString(us1) + " should not pass less than zero check")

      // {1,3,4,5,7,1000}
      val us3 = union(s1234, union(union(singletonSet(5), singletonSet(7)), singletonSet(FunSets.bound)))

      val lessThanFive = forall(us3, elem => elem < 5)
      assert(!lessThanFive, "lessThanFive should be false but got true")

    }
  }

  test("exists test") {
    new TestSets {

      val us3 = union(s1234, union(union(singletonSet(5), singletonSet(7)), singletonSet(FunSets.bound)))
      val containsElemsLessThan5 = exists(us3, elem => elem < 5)
//      FunSets.toString(us3)
      assert(containsElemsLessThan5, "exists for set " + FunSets.toString(us3) + " should contain elements less than 5")
    }
  }
}
