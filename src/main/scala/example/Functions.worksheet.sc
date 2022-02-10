// trait M[K] {
//   abstract class Entry extends M[K] {
//     val key: K
//   }
// }

// final class SEntry[K](key: K) extends M[K]().Entry[K] {}

// val e = new SEntry[Int](42)
// println(e.key)

def abs(n: Int) = if (n < 0) -n else n

abs(-5)

trait X
object X {
  final case object A extends X
  final case object B extends X
}

def f(x: X): PartialFunction[X, Unit] = { case X.A =>
  println("a")
}

// {_ + 1}(1)

val g1 = (x: Int) => x + 1
val g2 = List(1, 2) collect { case x: Int => x + 1 }

class S { s =>
  val x = 1
  val y = s.x + 1
  println(y)
}

val s = new S
s.y
