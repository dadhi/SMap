import org.scalatest._, org.scalatest.funsuite._
import example.map.SMap

class SMapSpec extends AnyFunSuite {
  test("Empty map is empty map") {
    assert(SMap.empty == SMap.empty)
  }
  test("Empty map isEmpty") {
    assert(SMap.empty.isEmpty)
  }
  test("map of one item is not Empty") {
    assert(SMap(1 -> 2).isEmpty == false)
  }
  test("Maps of two equal items are equal") {
    assert(SMap("a" -> 2) == SMap("a" -> 2))
  }
  test("Maps of two items is not equal to the map of one") {
    assert(SMap("a" -> 2, "b" -> 3) != SMap("a" -> 2))
  }
  test("Lookup for the existing key should succeed in map of one item") {
    assert(SMap("a" -> 2).getOrElse("a", 0) == 2)
  }
  test(
    "Adding two items with the same keys should keep the last item only (the same behavior as map)"
  ) {
    val m = SMap(1 -> "a", 1 -> "b")
    assert(m.size == 1)
    assert(m(1) == "b")
  }
  test(
    "Adding two items with the same hash but different keys should add both items"
  ) {
    val m = SMap(SameHash(1) -> "a", SameHash(2) -> "b")
    assert(m.size == 2)
    assert(m(SameHash(1)) == "a")
    assert(m(SameHash(2)) == "b")
  }
  test(
    "Adding two items with the same hash, then adding the new one with the same key should keep the new one"
  ) {
    val m = SMap(SameHash(1) -> "a", SameHash(2) -> "b", SameHash(2) -> "c")
    assert(m.size == 2)
    assert(m(SameHash(1)) == "a")
    assert(m(SameHash(2)) == "c")
  }
  test("Lookup for the non-existing key should succeed in map of one item") {
    assert(SMap("a" -> 2).get("b").isEmpty)
  }
  test("In map of N items I call lookup for all items") {
    val m = SMap(1 -> "a", 2 -> "b", 3 -> "c")
    assert(m(1) contains "a")
    assert(m(2) contains "b")
    assert(m(3) contains "c")
  }
  test("Remove from empty map should return empty map") {
    assert(SMap.empty.removed(1).isEmpty)
  }
  test("Remove existing key from 1 item map should return empty map") {
    assert(SMap(1 -> "a").removed(1).isEmpty)
  }
  test("Remove non-existing key from 1 item map should keep the original map") {
    val m = SMap(1 -> "a")
    assert(m.removed(2) eq m)
  }
  test(
    "Remove existing key from 2 item map should return the map without the key"
  ) {
    val m = SMap(1 -> "a", 2 -> "b")
    val r2 = m.removed(1)
    assert(r2.contains(1) == false)
    assert(r2.contains(2) == true)

    val r1 = m.removed(2)
    assert(r1.contains(1) == true)
    assert(r1.contains(2) == false)
  }
  test(
    "Remove existing key from 3 item map should return the map without the key"
  ) {
    val m = SMap(1 -> "a", 2 -> "b", 3 -> "c")
    val r = m.removed(2)
    assert(r.size == 2)
    assert(r.contains(1))
    assert(r.contains(3))
  }
  test("Can add 4, 5, 6, 7 items to the map") {
    val m4 = SMap(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "d")
    assert(m4.size == 4)
    assert(m4.contains(4))

    val m5 = m4 + (5, "e") 
    assert(m5.size == 5)
    assert(m5.contains(5))

    val m6 = m5 + (6, "f") 
    assert(m6.size == 6)
    assert(m6.contains(6))

    val m7 = m6 + (7, "g")
    assert(m7.size == 7)
    assert(m7.contains(7))
  }
  test("Can add 8, 9, 10, 11 items to the map") {
    var m = SMap.empty[Int, String]
    for (i <- 1 to 11)
      m = m.updated(i, s"v$i")
    assert(m.size == 11)
    // then update
    for (i <- 1 to 11)
      m = m.updated(i, s"u$i")
    assert(m.size == 11)
  }
  test("Can add 12-15 items to the map") {
    var m = SMap.empty[Int, String]
    for (i <- 1 to 11)
      m = m.updated(i, s"v$i")
    for (i <- 12 to 15)
      m = m.updated(i, s"v$i")
    assert(m.size == 15)
    // then update
    for (i <- 1 to 15)
      m = m.updated(i, s"u$i")
    assert(m.size == 15)
  }
}

case class SameHash(val n: Int) {
  override def hashCode = 1
  override def equals(obj: Any): Boolean = obj match {
    case SameHash(i) => i == n
    case _           => false
  }
}
