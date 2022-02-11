import org.scalatest.funsuite._
import example.map.SMap

class SMapSpec extends AnyFunSuite {
  test("Empty map is empty map") {
    assert(SMap.empty == SMap.empty)
  }
  test("Empty map isEmpty") {
    assert(SMap.empty.isEmpty)
  }
}
