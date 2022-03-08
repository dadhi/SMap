case class Foo[@specialized(Int) T](bar: T)
Foo(42).getClass.getName
Foo("").getClass.getName

trait M[@specialized(Int) K, V]
object M {
  abstract class E[K, V](i: Int) extends M[K, V]
  final case class KVE[K, V](i: Int, k: K) extends E[K, V](i)
  final case class VE[V](i: Int) extends E[Int, V](i)
}

val s = M.KVE(1, 1)
s.getClass.getName

List("0", "-1", "3", "1", "-2").map(_.toInt)

Map(1 -> 2).foreach { case (k, v) => println(s"$k -> $v") }
