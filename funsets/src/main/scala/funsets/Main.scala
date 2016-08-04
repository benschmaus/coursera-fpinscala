package funsets

object Main extends App {
  import FunSets._

  val s1 = singletonSet(1)

  println(contains(s1, 1))
  println(contains(s1, 2))

}
