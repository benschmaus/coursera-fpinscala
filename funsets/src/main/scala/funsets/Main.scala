package funsets

object Main extends App {
  import FunSets._

  println(new Bar().x);
}

abstract class Foo {
  val x = 1
}

class Bar extends Foo {
  override val x = 2
}