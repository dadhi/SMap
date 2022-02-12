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
    assert(SMap("a" -> 2).get("a").getOrElse(0) == 2)
  }
  test("Lookup for the non-existing key should succeed in Map of one item") {
    assert(SMap("a" -> 2).get("b").isEmpty)
  }
  // test("In Map of N items I call lookup for all items") {
  //   val m = SMap(1 -> "a", 2 -> "b", 3 -> "c")
  //   assert(m.get(1) contains "a")
  //   assert(m.get(2) contains "b")
  //   assert(m.get(3) contains "c")
  // }
}
