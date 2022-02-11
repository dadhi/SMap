import org.scalatest.funsuite._
import example.map.SHashMap

class SHashMapSpec extends AnyFunSuite {
  test("Empty map is empty map") {
    assert(SHashMap.empty == SHashMap.empty)
  }
  test("Empty map isEmpty") {
    assert(SHashMap.empty.isEmpty)
  }
}
