import org.scalatest._, org.scalatest.funsuite._
import example.map.SMap

class SMapSpec extends AnyFunSuite {
  test("Empty map is empty map") {
    assert(SMap.empty == SMap.empty)
  }
  test("Empty map isEmpty") {
    assert(SMap.empty.isEmpty)
  }
  test("Map of one item is not Empty") {
    assert(Map(1 -> 2).isEmpty == false)
  }
  test("Maps of two equal items are equal") {
    assert(SMap("a" -> 2) == SMap("a" -> 2))
  }
  test("Maps of two items is not equal to the Map of one") {
    assert(SMap("a" -> 2, "b" -> 3) != SMap("a" -> 2))
  }
  test("Lookup for the existing key should succeed in Map of one item") {
    assert(SMap("a" -> 2).getOrElse("a", 0) == 2)
  }
  test(
    "Adding two items with the same keys should keep the last item only (the same behavior as Map)"
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
  test("Lookup for the non-existing key should succeed in Map of one item") {
    assert(SMap("a" -> 2).get("b").isEmpty)
  }
  test("In Map of N items I call lookup for all items") {
    val m = SMap(1 -> "a", 2 -> "b", 3 -> "c")
    assert(m(1) contains "a")
    assert(m(2) contains "b")
    assert(m(3) contains "c")
  }
}

case class SameHash(val n: Int) {
  override def hashCode = 1
  override def equals(obj: Any): Boolean = obj match {
    case SameHash(i) => i == n
    case _           => false
  }
}
