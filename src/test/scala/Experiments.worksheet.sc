import speedy.SMap
val m = SMap.empty[Int, String]
val e = m.updated(1, "a")
e.getClass.getName

case class Foo[@specialized(Int) T](bar: T)
Foo(42).getClass.getName
Foo("").getClass.getName

trait M[@specialized(Int) K, V]
object M {
  abstract class E[K, V](i: Int) extends M[K, V]
  final case class KVE[K, V](i: Int, k: K) extends E[K, V](i)
  final case class VE[V](i: Int) extends E[Int, V](i)
}

Map(1 -> "a", 2 -> "b").foreach()

val s = M.KVE(1, 1)
s.getClass.getName
