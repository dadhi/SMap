import speedy.SMap
val m = SMap.empty[Int, String]
// val e = m.updated(1, "a")

//-------

case class XMap[K, +V](k: K, v: V) {
  def add[V1 >: V](key: K, value: V1): XMap[K, V1] = XMap(key, value)
}

class A(i: Int)
case class B(i: Int) extends A(i + 1)

val x = XMap[Int, A](0, new A(0))
val b = B(41)
val x1 = x.add(1, b)
