abstract class MyT {
  def get1 = 1
}

class ConcreteMyT(val s: String) extends MyT {

}

class ConcreteMyT2(_s: String) extends MyT {
  val s = _s
}


val c1 = new ConcreteMyT("foo")
val c2 = new ConcreteMyT2("bar")

c1.s
c2.s

c1.get1 + c2.get1


