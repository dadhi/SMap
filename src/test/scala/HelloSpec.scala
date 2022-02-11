import org.scalatest.funsuite._
import play.api.libs.json._

class HelloSpec extends AnyFunSuite {
  test("Hello should start with H") {
    assert("Hello".startsWith("H"))
  }

  test("Anonymous function") {
    val p = { Json.parse(_: String) }
    println(p.getClass.getSimpleName)
  }
}
