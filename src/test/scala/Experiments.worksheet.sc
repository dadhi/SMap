case class XMap[K, +V](k: K, v: V) {
  def add[V1 >: V](key: K, value: V1): XMap[K, V1] = XMap(key, value)
}

class A(i: Int)
case class B(i: Int) extends A(i + 1)

val m = XMap[Int, A](0, new A(0))
val b = B(41)
val m1 = m.add(1, b)