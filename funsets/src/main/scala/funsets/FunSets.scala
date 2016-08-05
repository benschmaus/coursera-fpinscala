package funsets


/**
 * 2. Purely Functional Sets.
  *
  * Code scores 10 of 10 against course tests.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
    def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = elemIn => elem == elemIn
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = elem => s(elem) || t(elem)

  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = elem => s(elem) && t(elem)
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = elem => {
      s(elem) && !t(elem)
    }
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = elem => s(elem) && p(elem)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int, state: Boolean): Boolean = {
      if (a > bound) {
//        println("passed upper bound so returning last checked state of " + state + " and terminating iteration")
        state
      }
      else if (s(a)) {
//        println("test for set member " + a + " is " + p(a))
        val lState = p(a)
        if (!lState) {
//          println("test failed for member " + a + " so returning false and exiting")
          false
        }
        else
          iter(a+1, lState)
      }
      else iter(a+1, state)
    }
    iter(-bound, false)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = forall(filter(s, p), p)
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = {
      def iter(a: Int, ns: Set): Set = {
        if (a > bound) ns
        else if (s(a)) union(iter(a+1, ns), singletonSet(f(a)))
        else iter(a+1, ns)
      }
      iter(-bound, singletonSet(bound+1))
    }
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
