import org.scalatest.funsuite._
import play.api.libs.json._
import example.map.SHashMap

class HelloSpec extends AnyFunSuite {
  test("Hello should start with H") {
    assert("Hello".startsWith("H"))
  }

  test("Anonymous function") {
    val p = { Json.parse(_: String) }
    println(p.getClass.getSimpleName)
  }

  test("Empty map is empty map") {
    assert(SHashMap.empty == SHashMap.empty)
  }
}
