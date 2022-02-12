import org.scalatest.funsuite._
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
    assert(Map("a" -> 2) == Map("a" -> 2))
  }
  test("Maps of two items is not equal to the Map of one") {
    assert(Map("a" -> 2, "b" -> 3) != Map("a" -> 2))
  }
}
